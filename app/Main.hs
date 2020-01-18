{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Control.Foldl as Foldl
import           Control.Concurrent.Async (forConcurrently_, mapConcurrently)
import           Control.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import           Control.Exception (bracket_)
import           Control.Monad (filterM)
import qualified Data.Aeson as Aeson
import           Data.Aeson.Encode.Pretty
import           Data.Either.Combinators (rightToMaybe)
import           Data.Foldable (fold, foldMap, traverse_)
import qualified Data.Graph as G
import           Data.List (maximumBy)
import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe)
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
import           Turtle hiding (arg, fold, s, x)
import qualified Turtle
import           Types (PackageName, mkPackageName, runPackageName, untitledPackageName, preludePackageName)

echoT :: Text -> IO ()
echoT = Turtle.printf (Turtle.s % "\n")

exitWithErr :: Text -> IO a
exitWithErr errText = errT errText >> exit (ExitFailure 1)
  where errT = traverse Turtle.err . textToLines

packageFile :: Path.FilePath
packageFile = "psc-package.json"

localPackageSet :: Path.FilePath
localPackageSet = "packages.json"

packageDir :: Text -> PackageName -> Text -> Turtle.FilePath
packageDir set pkgName version =
  ".psc-package" </> fromText set </> fromText (runPackageName pkgName) </> fromText version

-- a the source of a package set can be a git url or a local path
data UrlOrLocal
  = Url Text
  | Local Text
  deriving (Show, Generic)

-- is the given text a url, as far as we care about sources?
-- i.e. anything ending in ".git" seems to be fair game for git
isUrl :: Text -> Bool
isUrl txt =
  let endsInGit = T.isSuffixOf ".git" txt
  in endsInGit

mkUrlOrLocal :: Text -> UrlOrLocal
mkUrlOrLocal txt =
  if isUrl txt
    then Url txt
    else Local txt

instance Aeson.FromJSON UrlOrLocal where
  parseJSON value = mkUrlOrLocal <$> Aeson.parseJSON value

instance Aeson.ToJSON UrlOrLocal where
  toJSON (Url txt) = Aeson.toJSON txt
  toJSON (Local txt) = Aeson.toJSON txt

data PackageConfig = PackageConfig
  { name    :: PackageName
  , depends :: [PackageName]
  , set     :: Text
  , source  :: UrlOrLocal
  } deriving (Show, Generic, Aeson.FromJSON, Aeson.ToJSON)

pathToTextUnsafe :: Turtle.FilePath -> Text
pathToTextUnsafe = either (error "Path.toText failed") id . Path.toText

shellToIOText :: Turtle.Shell Line -> IO [Text]
shellToIOText shellLines = Turtle.fold (fmap lineToText shellLines) Foldl.list

readPackageFile :: IO PackageConfig
readPackageFile = do
  exists <- testfile packageFile
  unless exists $ exitWithErr "psc-package.json does not exist. Maybe you need to run psc-package init?"
  mpkg <- Aeson.eitherDecodeStrict . encodeUtf8 <$> readTextFile packageFile
  case mpkg of
    Left errors -> exitWithErr $ "Unable to parse psc-package.json: " <> T.pack errors
    Right pkg -> return pkg

packageConfigToJSON :: PackageConfig -> Text
packageConfigToJSON =
    TL.toStrict
    . TB.toLazyText
    . encodePrettyToTextBuilder' config
    . sortDependencies
  where
    config = defConfig
               { confCompare =
                   keyOrder [ "name"
                            , "set"
                            , "source"
                            , "depends"
                            ]
               , confIndent = Spaces 2
               , confTrailingNewline = True
               }
    sortDependencies conf = conf { depends = List.sort (depends conf) }

packageSetToJSON :: PackageSet -> Text
packageSetToJSON =
    TL.toStrict
    . TB.toLazyText
    . encodePrettyToTextBuilder' config
    . sortDependencies
  where
    config = defConfig
               { confCompare = compare
               , confIndent = Spaces 2
               , confTrailingNewline = True
               }
    sortDependencies set = updateDependencies <$> set
    updateDependencies pkg =
      pkg { dependencies = List.sort (dependencies pkg) }

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
  case source of
    Local _ -> pure ()
    Url source' -> do
      exists <- testdir pkgDir
      unless exists . void $ cloneShallow source' set pkgDir

readPackageSet :: PackageConfig -> IO PackageSet
readPackageSet PackageConfig{ set, source } = do
  case source of
    Local txt -> do
      handleReadPackageSet (fromText txt)
    Url _ -> do
      let dbFile = ".psc-package" </> fromText set </> ".set" </> "packages.json"
      handleReadPackageSet dbFile

handleReadPackageSet :: Path.FilePath -> IO PackageSet
handleReadPackageSet dbFile = do
  exists <- testfile dbFile
  unless exists $ exitWithErr $ format (fp%" does not exist") dbFile
  mdb <- Aeson.eitherDecodeStrict . encodeUtf8 <$> readTextFile dbFile
  case mdb of
    Left errors -> exitWithErr $ "Unable to parse packages.json: " <> T.pack errors
    Right db -> return db

writePackageSet :: PackageConfig -> PackageSet -> IO ()
writePackageSet PackageConfig{ set, source } =
  case source of
    Local txt -> writeTextFile (fromText txt) . packageSetToJSON
    Url _ ->
      let dbFile = ".psc-package" </> fromText set </> ".set" </> "packages.json"
      in writeTextFile dbFile . packageSetToJSON

readLocalPackageSet :: IO PackageSet
readLocalPackageSet = handleReadPackageSet localPackageSet

writeLocalPackageSet :: PackageSet -> IO ()
writeLocalPackageSet = writeTextFile localPackageSet . packageSetToJSON

performInstall :: Text -> PackageName -> PackageInfo -> IO Turtle.FilePath
performInstall set pkgName PackageInfo{ repo, version } = do
  let pkgDir = packageDir set pkgName version
  exists <- testdir pkgDir
  unless exists . void $ do
    echoT ("Installing " <> runPackageName pkgName)
    cloneShallow repo version pkgDir
  pure pkgDir

getReverseDeps  :: PackageSet -> PackageName -> IO [(PackageName, PackageInfo)]
getReverseDeps db dep =
    List.nub <$> foldMap go (Map.toList db)
  where
    go pair@(packageName, PackageInfo {dependencies}) =
      case List.find (== dep) dependencies of
        Nothing -> return mempty
        Just _ -> do
          innerDeps <- getReverseDeps db packageName
          return $ pair : innerDeps

getPursPath :: IO Text
getPursPath = do
  bin <- which "purs" -- *nix binary
  exe <- which "purs.exe" -- windows binary (maybe it putted directly on the PATH)
  cmd <- which "purs.cmd" -- windows binary wrapper (maybe it built with npm)
  let purs = bin <|> exe <|> cmd
  case purs of
    Nothing -> exitWithErr "The \"purs\" executable could not be found. Please make sure your PATH variable is set correctly"
    Just p -> return $ pathToTextUnsafe p

getTransitiveDeps :: PackageSet -> [PackageName] -> IO [(PackageName, PackageInfo)]
getTransitiveDeps db deps =
    Map.toList . fold <$> traverse (go Set.empty) deps
  where
    go seen pkg
      | pkg `Set.member` seen =
          exitWithErr ("Cycle in package dependencies at package " <> runPackageName pkg)
      | otherwise =
        case Map.lookup pkg db of
          Nothing ->
            exitWithErr (pkgNotFoundMsg pkg)
          Just info@PackageInfo{ dependencies } -> do
            m <- fold <$> traverse (go (Set.insert pkg seen)) dependencies
            return (Map.insert pkg info m)

    pkgNotFoundMsg pkg =
      "Package `" <> runPackageName pkg <> "` does not exist in package set" <> extraHelp
      where
        extraHelp = case suggestedPkg of
          Just pkg' | Map.member pkg' db ->
            " (but `" <> runPackageName pkg' <> "` does, did you mean that instead?)"
          Just pkg' ->
            " (and nor does `" <> runPackageName pkg' <> "`)"
          Nothing ->
            ""

        suggestedPkg = do
          sansPrefix <- T.stripPrefix "purescript-" (runPackageName pkg)
          rightToMaybe (mkPackageName sansPrefix)

installImpl :: PackageConfig -> Maybe Int -> IO ()
installImpl config@PackageConfig{ depends } limitJobs = do
  getPackageSet config
  db <- readPackageSet config
  newPkgs <- getNewPackages db
  when (length newPkgs > 1) $ do
    echoT ("Installing " <> pack (show (length newPkgs)) <> " new packages...")
  case limitJobs of
    Nothing ->
      forConcurrently_ newPkgs .  uncurry $ performInstall $ set config
    Just max' -> do
      sem <- newQSem max'
      forConcurrently_ newPkgs .  uncurry . (\x y z -> bracket_ (waitQSem sem) (signalQSem sem) (performInstall x y z)) $ set config
  where
    getNewPackages db =
      getTransitiveDeps db depends >>= filterM isNewPackage

    isNewPackage (name, info) =
      fmap not $ testdir $ packageDir (set config) name (version info)

getPureScriptVersion :: IO Version
getPureScriptVersion = do
  pursPath <- getPursPath
  let pursProc = inproc pursPath [ "--version" ] empty
  outputLines <- shellToIOText pursProc
  case outputLines of
    [onlyLine]
      | results@(_ : _) <- Read.readP_to_S parseVersion (T.unpack onlyLine) ->
           pure (fst (maximumBy (comparing (length . versionBranch . fst)) results))
      | otherwise -> exitWithErr "Unable to parse output of purs --version"
    _ -> exitWithErr "Unexpected output from purs --version"

initialize :: Maybe (Text, Maybe Text) -> Maybe Int -> IO ()
initialize setAndSource limitJobs = do
    exists <- testfile "psc-package.json"
    when exists $ exitWithErr "psc-package.json already exists"
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
                           , source  = Url "https://github.com/purescript/package-sets.git"
                           , set     = "psc-" <> pack (showVersion pursVersion)
                           }
      Just (set, source) ->
        pure PackageConfig { name    = pkgName
                           , depends = [ preludePackageName ]
                           , source  = case source of
                               Just x -> mkUrlOrLocal x
                               Nothing -> Url "https://github.com/purescript/package-sets.git"
                           , set
                           }

    writePackageFile pkg
    installImpl pkg limitJobs
  where
    packageNameFromPWD =
      either (const untitledPackageName) id . mkPackageName

install :: Maybe String -> Maybe Int -> IO ()
install pkgName' limitJobs = do
  pkg <- readPackageFile
  case pkgName' of
    Nothing -> do
      installImpl pkg limitJobs
      echoT "Install complete"
    Just str -> do
      pkgName <- packageNameFromString str
      let pkg' = pkg { depends = List.nub (pkgName : depends pkg) }
      updateAndWritePackageFile pkg' limitJobs

uninstall :: String -> Maybe Int -> IO ()
uninstall pkgName' limitJobs = do
  pkg <- readPackageFile
  pkgName <- packageNameFromString pkgName'
  let pkg' = pkg { depends = filter (/= pkgName) $ depends pkg }
  updateAndWritePackageFile pkg' limitJobs

updateAndWritePackageFile :: PackageConfig -> Maybe Int -> IO ()
updateAndWritePackageFile pkg limitJobs = do
  installImpl pkg limitJobs
  writePackageFile pkg
  echoT "psc-package.json file was updated"

packageNameFromString :: String -> IO PackageName
packageNameFromString str =
  case mkPackageName (pack str) of
    Right pkgName ->
      pure pkgName
    Left _ -> exitWithErr $ "Invalid package name: " <> pack (show str)

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
  let paths = [ packageDir set pkgName version </> "src" </> "**" </> "*.purs"
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
exec :: [String] -> Bool -> [String] -> Maybe Int -> IO ()
exec execNames onlyDeps passthroughOptions limitJobs = do
  pkg <- readPackageFile
  installImpl pkg limitJobs

  pursPath <- getPursPath
  paths <- getPaths
  let cmdName = head execNames
      cmdParts = tail execNames
      srcParts = [ "src" </> "**" </> "*.purs" | not onlyDeps ]
  exit
    =<< Process.waitForProcess
    =<< Process.runProcess
          (if cmdName == "purs" then T.unpack pursPath else cmdName)
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

data VerifyArgs a = Package a | VerifyAll (Maybe a) deriving (Functor, Foldable, Traversable)

verify :: VerifyArgs Text -> Maybe Int -> IO ()
verify arg limitJobs = do
  pkg <- readPackageFile
  db  <- readPackageSet pkg
  case traverse mkPackageName arg of
    Left pnError -> echoT . pack $ "Error while parsing arguments to verify: " <> show pnError
    Right (Package pName) -> case Map.lookup pName db of
      Nothing -> echoT . pack $ "No packages found with the name " <> show (runPackageName pName)
      Just _  -> do
        reverseDeps <- map fst <$> getReverseDeps db pName
        let packages = pure pName <> reverseDeps
        verifyPackages packages db pkg

    Right (VerifyAll pName) -> verifyPackages packages db pkg
      where
        packages = Map.keys $ maybe db pFilter pName
        pFilter name = Map.filterWithKey (\k _ -> runPackageName k >= runPackageName name) db

  where
    verifyPackages :: [PackageName] -> PackageSet -> PackageConfig -> IO ()
    verifyPackages names db pkg = do
      echoT $ "Verifying " <> pack (show $ length names) <> " packages."
      echoT "Warning: this could take some time!"
      traverse_ (verifyPackage db pkg) names

    verifyPackage :: PackageSet -> PackageConfig -> PackageName -> IO ()
    verifyPackage db pkg name = do
      let
        dirFor pkgName =
          case Map.lookup pkgName db of
            Nothing -> error ("verifyPackageSet: no directory for " <> show pkgName)
            Just pkgInfo -> performInstall (set pkg) pkgName pkgInfo
      echoT ("Verifying package " <> runPackageName name)
      dependencies <- map fst <$> getTransitiveDeps db [name]
      dirs <- case limitJobs of
        Nothing -> mapConcurrently dirFor dependencies
        Just max' -> do
          sem <- newQSem max'
          mapConcurrently (bracket_ (waitQSem sem) (signalQSem sem) . dirFor) dependencies
      let srcGlobs = map (pathToTextUnsafe . (</> ("src" </> "**" </> "*.purs"))) dirs
      pursPath <- getPursPath
      procs pursPath ("compile" : srcGlobs) empty

formatPackageFile :: IO ()
formatPackageFile =
    readLocalPackageSet >>= writeLocalPackageSet

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
                                   <*> optional limitJobs
                                   Opts.<**> Opts.helper)
            (Opts.progDesc "Create a new psc-package.json file"))
        , Opts.command "uninstall"
            (Opts.info (uninstall <$> pkg <*> optional limitJobs Opts.<**> Opts.helper)
            (Opts.progDesc "Uninstall the named package"))
        , Opts.command "install"
            (Opts.info (install <$> optional pkg <*> optional limitJobs Opts.<**> Opts.helper)
            (Opts.progDesc "Install/update the named package and add it to 'depends' if not already listed. If no package is specified, install/update all dependencies."))
        , Opts.command "build"
            (Opts.info (exec ["purs", "compile"]
                        <$> onlyDeps "Compile only the package's dependencies"
                        <*> passthroughArgs "purs compile"
                        <*> optional limitJobs
                        Opts.<**> Opts.helper)
            (Opts.progDesc "Install dependencies and compile the current package"))
        , Opts.command "repl"
            (Opts.info (exec ["purs", "repl"]
                        <$> onlyDeps "Load only the package's dependencies"
                        <*> passthroughArgs "purs repl"
                        <*> optional limitJobs
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
        , Opts.command "verify"
            (Opts.info (verify <$>
                        ((Package . fromString <$> pkg)
                         <|> (VerifyAll <$> optional (fromString <$> after)))
                        <*> optional limitJobs
                        Opts.<**> Opts.helper)
            (Opts.progDesc "Verify that the named package builds correctly. If no package is specified, verify that all packages in the package set build correctly."))
        , Opts.command "format"
            (Opts.info (pure formatPackageFile)
            (Opts.progDesc "Format the packages.json file for consistency"))
        ]
      where
        pkg = Opts.strArgument $
             Opts.metavar "PACKAGE"
          <> Opts.help "The name of the package to install"

        limitJobs = Opts.option Opts.auto $
             Opts.long "jobs"
          <> Opts.help "Limit the number of jobs that can run concurrently"

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

        after = Opts.strOption $
             Opts.long "after"
          <> Opts.help "Skip packages before this package during verification"
