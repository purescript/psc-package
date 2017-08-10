{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.PureScript.Package.Initialize (initialize) where

import qualified Control.Foldl as Foldl
import           Data.List (maximumBy)
import           Data.Maybe (fromMaybe)
import           Data.Ord (comparing)
import qualified Data.Text as T
import           Data.Version (Version(..), parseVersion, showVersion)
import           Text.ParserCombinators.ReadP (readP_to_S)
import           Turtle
import           Prelude hiding (FilePath)

import Language.PureScript.Package.Types.PackageConfig (PackageConfig(..), name, depends, set, source, writePackageFile)
import Language.PureScript.Package.Types.PackageName (untitledPackageName, mkPackageName, preludePackageName)
import Language.PureScript.Package.Path (pathToTextUnsafe)
import Language.PureScript.Package.Update (updateImpl)

initialize :: Maybe (Text, Maybe Text) -> IO ()
initialize setAndSource = do
    exists <- testfile "psc-package.json"
    when exists $ die "psc-package.json already exists"
    echo "Initializing new project in current directory"
    pkgName <- packageNameFromPWD . pathToTextUnsafe . filename <$> pwd
    pkg <- case setAndSource of
      Nothing -> do
        pursVersion <- getPureScriptVersion
        echo ("Using the default package set for PureScript compiler version " <>
          unsafeTextToLine (fromString (showVersion pursVersion)))
        echo "(Use --source / --set to override this behavior)"
        pure PackageConfig { name    = pkgName
                           , depends = [ preludePackageName ]
                           , source  = defaultSource
                           , set     = "psc-" <> T.pack (showVersion pursVersion)
                           }
      Just (set, source) ->
        pure PackageConfig { name    = pkgName
                           , depends = [ preludePackageName ]
                           , source  = fromMaybe defaultSource source
                           , set
                           }

    writePackageFile pkg
    updateImpl pkg
  where
    defaultSource = "https://github.com/purescript/package-sets.git"
    packageNameFromPWD = either (const untitledPackageName) id . mkPackageName
    getPureScriptVersion = do
      let pursProc = inproc "purs" [ "--version" ] empty
      outputLines <- Turtle.fold (fmap lineToText pursProc) Foldl.list
      case outputLines of
        [onlyLine]
          | results@(_ : _) <- readP_to_S parseVersion (T.unpack onlyLine) ->
               pure (fst (maximumBy (comparing (length . versionBranch . fst)) results))
          | otherwise -> die "Unable to parse output of purs --version"
        _ -> die "Unexpected output from purs --version"
