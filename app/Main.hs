{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main where

import qualified Control.Foldl as Foldl
import           Control.Concurrent.Async (forConcurrently_)
import qualified Data.Aeson as Aeson
import           Data.Aeson.Encode.Pretty
import           Data.Foldable (fold, for_, traverse_)
import qualified Data.Graph as G
import           Data.List (maximumBy, nub)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Ord (comparing)
import qualified Data.Set as Set
import           Data.Text (pack)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Read as TR
import           Data.Traversable (for)
import           Data.Version (Version(..), parseVersion, showVersion)
import qualified Filesystem.Path.CurrentOS as Path
import           GHC.Generics (Generic)
import qualified Options.Applicative as Opts
import qualified Paths_psc_package as Paths
import           System.Environment (getArgs)
import qualified System.IO as IO
import qualified System.Process as Process
import qualified Text.ParserCombinators.ReadP as Read
import           Turtle hiding (echo, fold, s, x)
import qualified Turtle
import           Types (PackageName, mkPackageName, runPackageName, untitledPackageName, preludePackageName)

echoT :: Text -> IO ()
echoT = Turtle.printf (Turtle.s % "\n")

packageFile :: Path.FilePath
packageFile = "psc-package.json"

data PackageConfig = PackageConfig
  { name    :: PackageName
  , depends :: [PackageName]
  , set     :: Text
  , source  :: Text
  } deriving (Show, Generic, Aeson.FromJSON, Aeson.ToJSON)

pathToTextUnsafe :: Turtle.FilePath -> Text
pathToTextUnsafe = either (error "Path.toText failed") id . Path.toText

readPackageFile :: IO PackageConfig
readPackageFile = do
  exists <- testfile packageFile
  unless exists $ do
    echoT "psc-package.json does not exist. Maybe you need to run psc-package init?"
    exit (ExitFailure 1)
  mpkg <- Aeson.decodeStrict . encodeUtf8 <$> readTextFile packageFile
  case mpkg of
    Nothing -> do
      echoT "Unable to parse psc-package.json"
      exit (ExitFailure 1)
    Just pkg -> return pkg

packageConfigToJSON :: PackageConfig -> Text
packageConfigToJSON =
    TL.toStrict
    . TB.toLazyText
    . encodePrettyToTextBuilder' config
  where
    config = defConfig
               { confCompare =
                   keyOrder [ "name"
                            , "set"
                            , "source"
                            , "depends"
                            ]
               }

packageSetToJSON :: PackageSet -> Text
packageSetToJSON =
    TL.toStrict
    . TB.toLazyText
    . encodePrettyToTextBuilder' config
  where
    config = defConfig { confCompare = compare }

writePackageFile :: PackageConfig -> IO ()
writePackageFile =
  writeTextFile packageFile
  . packageConfigToJSON

data PackageInfo = PackageInfo
  { repo         :: Text
  , version      :: Text
  , dependencies :: [PackageName]
  } deriving (Show, Eq, Generic, Aeson.FromJSON, Aeson.ToJSON)

type PackageSet = Map.Map PackageName PackageInfo

cloneShallow
  :: Text
  -- ^ repo
  -> Text
  -- ^ branch/tag
  -> Turtle.FilePath
  -- ^ target directory
  -> IO ExitCode
cloneShallow from ref into =
  proc "git"
       [ "clone"
       , "-q"
       , "-c", "advice.detachedHead=false"
       , "--depth", "1"
       , "-b", ref
       , from
       , pathToTextUnsafe into
       ] empty .||. exit (ExitFailure 1)

listRemoteTags
  :: Text
  -- ^ repo
  -> Turtle.Shell Text
listRemoteTags from = let gitProc = inproc "git"
                                    [ "ls-remote"
                                    , "-q"
                                    , "-t"
                                    , from
                                    ] empty
                      in lineToText <$> gitProc

getPackageSet :: PackageConfig -> IO ()
getPackageSet PackageConfig{ source, set } = do
  let pkgDir = ".psc-package" </> fromText set </> ".set"
  exists <- testdir pkgDir
  unless exists . void $ cloneShallow source set pkgDir

readPackageSet :: PackageConfig -> IO PackageSet
readPackageSet PackageConfig{ set } = do
  let dbFile = ".psc-package" </> fromText set </> ".set" </> "packages.json"
  exists <- testfile dbFile
  unless exists $ do
    echoT $ format (fp%" does not exist") dbFile
    exit (ExitFailure 1)
  mdb <- Aeson.decodeStrict . encodeUtf8 <$> readTextFile dbFile
  case mdb of
    Nothing -> do
      echoT "Unable to parse packages.json"
      exit (ExitFailure 1)
    Just db -> return db

writePackageSet :: PackageConfig -> PackageSet -> IO ()
writePackageSet PackageConfig{ set } =
  let dbFile = ".psc-package" </> fromText set </> ".set" </> "packages.json"
  in writeTextFile dbFile . packageSetToJSON

installOrUpdate :: Text -> PackageName -> PackageInfo -> IO Turtle.FilePath
installOrUpdate set pkgName PackageInfo{ repo, version } = do
  let pkgDir = ".psc-package" </> fromText set </> fromText (runPackageName pkgName) </> fromText version
  exists <- testdir pkgDir
  unless exists . void $ do
    echoT ("Updating " <> runPackageName pkgName)
    cloneShallow repo version pkgDir
  pure pkgDir

getTransitiveDeps :: PackageSet -> [PackageName] -> IO [(PackageName, PackageInfo)]
getTransitiveDeps db deps =
    Map.toList . fold <$> traverse (go Set.empty) deps
  where
    go seen pkg
      | pkg `Set.member` seen = do
          echoT ("Cycle in package dependencies at package " <> runPackageName pkg)
          exit (ExitFailure 1)
      | otherwise =
        case Map.lookup pkg db of
          Nothing -> do
            echoT ("Package " <> runPackageName pkg <> " does not exist in package set")
            exit (ExitFailure 1)
          Just info@PackageInfo{ dependencies } -> do
            m <- fold <$> traverse (go (Set.insert pkg seen)) dependencies
            return (Map.insert pkg info m)

updateImpl :: PackageConfig -> IO ()
updateImpl config@PackageConfig{ depends } = do
  getPackageSet config
  db <- readPackageSet config
  trans <- getTransitiveDeps db depends
  echoT ("Updating " <> pack (show (length trans)) <> " packages...")
  forConcurrently_ trans . uncurry $ installOrUpdate (set config)

getPureScriptVersion :: IO Version
getPureScriptVersion = do
  let pursProc = inproc "purs" [ "--version" ] empty
  outputLines <- Turtle.fold (fmap lineToText pursProc) Foldl.list
  case outputLines of
    [onlyLine]
      | results@(_ : _) <- Read.readP_to_S parseVersion (T.unpack onlyLine) ->
           pure (fst (maximumBy (comparing (length . versionBranch . fst)) results))
      | otherwise ->
           echoT "Unable to parse output of purs --version" >> exit (ExitFailure 1)
    _ -> echoT "Unexpected output from purs --version" >> exit (ExitFailure 1)

initialize :: Maybe (Text, Maybe Text) -> IO ()
initialize setAndSource = do
    exists <- testfile "psc-package.json"
    when exists $ do
      echoT "psc-package.json already exists"
      exit (ExitFailure 1)
    echoT "Initializing new project in current directory"
    pkgName <- packageNameFromPWD . pathToTextUnsafe . Path.filename <$> pwd
    pkg <- case setAndSource of
      Nothing -> do
        pursVersion <- getPureScriptVersion
        echoT ("Using the default package set for PureScript compiler version " <>
          fromString (showVersion pursVersion))
        echoT "(Use --source / --set to override this behavior)"
        pure PackageConfig { name    = pkgName
                           , depends = [ preludePackageName ]
                           , source  = "https://github.com/purescript/package-sets.git"
                           , set     = "psc-" <> pack (showVersion pursVersion)
                           }
      Just (set, source) ->
        pure PackageConfig { name    = pkgName
                           , depends = [ preludePackageName ]
                           , source  = fromMaybe "https://github.com/purescript/package-sets.git" source
                           , set
                           }

    writePackageFile pkg
    updateImpl pkg
  where
    packageNameFromPWD =
      either (const untitledPackageName) id . mkPackageName

update :: IO ()
update = do
  pkg <- readPackageFile
  updateImpl pkg
  echoT "Update complete"

install :: String -> IO ()
install pkgName' = do
  pkg <- readPackageFile
  pkgName <- packageNameFromString pkgName'
  let pkg' = pkg { depends = nub (pkgName : depends pkg) }
  updateAndWritePackageFile pkg'

uninstall :: String -> IO ()
uninstall pkgName' = do
  pkg <- readPackageFile
  pkgName <- packageNameFromString pkgName'
  let pkg' = pkg { depends = filter (/= pkgName) $ depends pkg }
  updateAndWritePackageFile pkg'

updateAndWritePackageFile :: PackageConfig -> IO ()
updateAndWritePackageFile pkg = do
  updateImpl pkg
  writePackageFile pkg
  echoT "psc-package.json file was updated"

packageNameFromString :: String -> IO PackageName
packageNameFromString str =
  case mkPackageName (pack str) of
    Right pkgName ->
      pure pkgName
    Left _ -> do
      echoT ("Invalid package name: " <> pack (show str))
      exit (ExitFailure 1)

listDependencies :: IO ()
listDependencies = do
  pkg@PackageConfig{ depends } <- readPackageFile
  db <- readPackageSet pkg
  trans <- getTransitiveDeps db depends
  traverse_ (echoT . runPackageName . fst) trans

listPackages :: Bool -> IO ()
listPackages sorted = do
  pkg <- readPackageFile
  db <- readPackageSet pkg
  if sorted
    then traverse_ echoT (fmt <$> inOrder (Map.assocs db))
    else traverse_ echoT (fmt <$> Map.assocs db)
  where
  fmt :: (PackageName, PackageInfo) -> Text
  fmt (name, PackageInfo{ version, repo }) =
    runPackageName name <> " (" <> version <> ", " <> repo <> ")"

  inOrder xs = fromNode . fromVertex <$> vs where
    (gr, fromVertex) =
      G.graphFromEdges' [ (pkg, name, dependencies pkg)
                        | (name, pkg) <- xs
                        ]
    vs = G.topSort (G.transposeG gr)
    fromNode (pkg, name, _) = (name, pkg)

getSourcePaths :: PackageConfig -> PackageSet -> [PackageName] -> IO [Turtle.FilePath]
getSourcePaths PackageConfig{..} db pkgNames = do
  trans <- getTransitiveDeps db pkgNames
  let paths = [ ".psc-package"
                </> fromText set
                </> fromText (runPackageName pkgName)
                </> fromText version
                </> "src" </> "**" </> "*.purs"
              | (pkgName, PackageInfo{ version }) <- trans
              ]
  return paths

getPaths :: IO [Turtle.FilePath]
getPaths = do
  pkg@PackageConfig{..} <- readPackageFile
  db <- readPackageSet pkg
  getSourcePaths pkg db depends

listSourcePaths :: IO ()
listSourcePaths = do
  paths <- getPaths
  traverse_ (echoT . pathToTextUnsafe) paths

-- | Helper for calling through to @purs@
--
-- Extra args will be appended to the options
exec :: [String] -> Bool -> [String] -> IO ()
exec execNames onlyDeps passthroughOptions = do
  pkg <- readPackageFile
  updateImpl pkg

  paths <- getPaths
  let cmdParts = tail execNames
      srcParts = [ "src" </> "**" </> "*.purs" | not onlyDeps ]
  exit
    =<< Process.waitForProcess
    =<< Process.runProcess
          (head execNames)
          (cmdParts <> passthroughOptions
                    <> map Path.encodeString (srcParts <> paths))
          Nothing -- no special path to the working dir
          Nothing -- no env vars
          Nothing -- use existing stdin
          Nothing -- use existing stdout
          Nothing -- use existing stderr

checkForUpdates :: Bool -> Bool -> IO ()
checkForUpdates applyMinorUpdates applyMajorUpdates = do
    pkg <- readPackageFile
    db <- readPackageSet pkg

    echoT ("Checking " <> pack (show (Map.size db)) <> " packages for updates.")
    echoT "Warning: this could take some time!"

    newDb <- Map.fromList <$> for (Map.toList db) (\(name, p@PackageInfo{ repo, version }) -> do
      echoT ("Checking package " <> runPackageName name)
      tagLines <- Turtle.fold (listRemoteTags repo) Foldl.list
      let tags = mapMaybe parseTag tagLines
      newVersion <- case parsePackageVersion version of
        Just parts ->
          let applyMinor =
                case filter (isMinorReleaseFrom parts) tags of
                  [] -> pure version
                  minorReleases -> do
                    echoT "New minor release available"
                    if applyMinorUpdates
                      then do
                        let latestMinorRelease = maximum minorReleases
                        pure ("v" <> T.intercalate "." (map (pack . show) latestMinorRelease))
                      else pure version
              applyMajor =
                case filter (isMajorReleaseFrom parts) tags of
                  [] -> applyMinor
                  newReleases -> do
                    echoT "New major release available"
                    if applyMajorUpdates
                      then do
                        let latestRelease = maximum newReleases
                        pure ("v" <> T.intercalate "." (map (pack . show) latestRelease))
                      else applyMinor
          in applyMajor
        _ -> do
          echoT "Unable to parse version string"
          pure version
      pure (name, p { version = newVersion }))

    when (applyMinorUpdates || applyMajorUpdates)
      (writePackageSet pkg newDb)
  where
    parseTag :: Text -> Maybe [Int]
    parseTag line =
      case T.splitOn "\t" line of
        [_sha, ref] ->
          case T.stripPrefix "refs/tags/" ref of
            Just tag ->
              case parsePackageVersion tag of
                Just parts -> pure parts
                _ -> Nothing
            _ -> Nothing
        _ -> Nothing

    parsePackageVersion :: Text -> Maybe [Int]
    parsePackageVersion ref =
      case T.stripPrefix "v" ref of
        Just tag ->
          traverse parseDecimal (T.splitOn "." tag)
        _ -> Nothing

    parseDecimal :: Text -> Maybe Int
    parseDecimal s =
      case TR.decimal s of
        Right (n, "") -> Just n
        _ -> Nothing

    isMajorReleaseFrom :: [Int] -> [Int] -> Bool
    isMajorReleaseFrom (0 : xs) (0 : ys) = isMajorReleaseFrom xs ys
    isMajorReleaseFrom (x : _)  (y : _)  = y > x
    isMajorReleaseFrom _        _        = False

    isMinorReleaseFrom :: [Int] -> [Int] -> Bool
    isMinorReleaseFrom (0 : xs) (0 : ys) = isMinorReleaseFrom xs ys
    isMinorReleaseFrom (x : xs) (y : ys) = y == x && ys > xs
    isMinorReleaseFrom _        _        = False

verifyPackageSet :: IO ()
verifyPackageSet = do
  pkg <- readPackageFile
  db <- readPackageSet pkg

  echoT ("Verifying " <> pack (show (Map.size db)) <> " packages.")
  echoT "Warning: this could take some time!"

  let installOrUpdate' (name, pkgInfo) = (name, ) <$> installOrUpdate (set pkg) name pkgInfo
  paths <- Map.fromList <$> traverse installOrUpdate' (Map.toList db)

  for_ (Map.toList db) $ \(name, _) -> do
    let dirFor pkgName = fromMaybe (error ("verifyPackageSet: no directory for " <> show pkgName)) (Map.lookup pkgName paths)
    echoT ("Verifying package " <> runPackageName name)
    dependencies <- map fst <$> getTransitiveDeps db [name]
    let srcGlobs = map (pathToTextUnsafe . (</> ("src" </> "**" </> "*.purs")) . dirFor) dependencies
    procs "purs" ("compile" : srcGlobs) empty

main :: IO ()
main = do
    IO.hSetEncoding IO.stdout IO.utf8
    IO.hSetEncoding IO.stderr IO.utf8
    join $ Opts.handleParseResult . execParserPure opts =<< getArgs
  where
    opts        = Opts.info (versionInfo <*> Opts.helper <*> commands) infoModList
    infoModList = Opts.fullDesc <> headerInfo <> footerInfo
    headerInfo  = Opts.progDesc "Manage package dependencies"
    footerInfo  = Opts.footer $ "psc-package " ++ showVersion Paths.version

    -- | Displays full command help when invoked with no arguments.
    execParserPure :: Opts.ParserInfo a -> [String] -> Opts.ParserResult a
    execParserPure pinfo [] = Opts.Failure $
      Opts.parserFailure Opts.defaultPrefs pinfo Opts.ShowHelpText mempty
    execParserPure pinfo args = Opts.execParserPure Opts.defaultPrefs pinfo args

    versionInfo :: Parser (a -> a)
    versionInfo = Opts.abortOption (Opts.InfoMsg (showVersion Paths.version)) $
      Opts.long "version" <> Opts.help "Show the version number" <> Opts.hidden

    commands :: Parser (IO ())
    commands = (Opts.subparser . fold)
        [ Opts.command "init"
            (Opts.info (initialize <$> optional ((,) <$> (fromString <$> set)
                                                     <*> optional (fromString <$> source))
                                   Opts.<**> Opts.helper)
            (Opts.progDesc "Initialize a new package"))
        , Opts.command "update"
            (Opts.info (pure update)
            (Opts.progDesc "Update dependencies"))
        , Opts.command "uninstall"
            (Opts.info (uninstall <$> pkg Opts.<**> Opts.helper)
            (Opts.progDesc "Uninstall the named package"))
        , Opts.command "install"
            (Opts.info (install <$> pkg Opts.<**> Opts.helper)
            (Opts.progDesc "Install the named package"))
        , Opts.command "build"
            (Opts.info (exec ["purs", "compile"]
                        <$> onlyDeps "Compile only the package's dependencies"
                        <*> passthroughArgs "purs compile"
                        Opts.<**> Opts.helper)
            (Opts.progDesc "Build the current package and dependencies"))
        , Opts.command "repl"
            (Opts.info (exec ["purs", "repl"]
                        <$> onlyDeps "Load only the package's dependencies"
                        <*> passthroughArgs "purs repl"
                        Opts.<**> Opts.helper)
            (Opts.progDesc "Open an interactive environment for PureScript"))
        , Opts.command "dependencies"
            (Opts.info (pure listDependencies)
            (Opts.progDesc "List all (transitive) dependencies for the current package"))
        , Opts.command "sources"
            (Opts.info (pure listSourcePaths)
            (Opts.progDesc "List all (active) source paths for dependencies"))
        , Opts.command "available"
            (Opts.info (listPackages <$> sorted Opts.<**> Opts.helper)
            (Opts.progDesc "List all packages available in the package set"))
        , Opts.command "updates"
            (Opts.info (checkForUpdates <$> apply <*> applyMajor Opts.<**> Opts.helper)
            (Opts.progDesc "Check all packages in the package set for new releases"))
        , Opts.command "verify-set"
            (Opts.info (pure verifyPackageSet)
            (Opts.progDesc "Verify that the packages in the package set build correctly"))
        ]
      where
        pkg = Opts.strArgument $
             Opts.metavar "PACKAGE"
          <> Opts.help "The name of the package to install"

        source = Opts.strOption $
             Opts.long "source"
          <> Opts.help "The Git repository for the package set"

        set = Opts.strOption $
             Opts.long "set"
          <> Opts.help "The package set tag name"

        apply = Opts.switch $
             Opts.long "apply"
          <> Opts.help "Apply all minor package updates"

        applyMajor = Opts.switch $
             Opts.long "apply-breaking"
          <> Opts.help "Apply all major package updates"

        onlyDeps help = Opts.switch $
             Opts.long "only-dependencies"
          <> Opts.short 'd'
          <> Opts.help help

        passthroughArgs cmd = many $ Opts.strArgument $
             Opts.help ("Options passed through to " <> cmd <> "; use -- to separate")
          <> Opts.metavar ("`" <> cmd <> "`" <> "-options")

        sorted = Opts.switch $
             Opts.long "sort"
          <> Opts.short 's'
          <> Opts.help "Sort packages in dependency order"
