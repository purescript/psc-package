{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Language.PureScript.Package.Verify (verifyPackageSet) where

import           Data.Foldable (for_)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Turtle
import           Prelude hiding (FilePath)

import Language.PureScript.Package.Types.PackageConfig (readPackageFile, set)
import Language.PureScript.Package.Types.PackageName (runPackageName)
import Language.PureScript.Package.Types.PackageSet (readPackageSet, getTransitiveDeps)
import Language.PureScript.Package.Path (pathToTextUnsafe)
import Language.PureScript.Package.Update (installOrUpdate)

verifyPackageSet :: IO ()
verifyPackageSet = do
  pkg <- readPackageFile
  db <- readPackageSet pkg

  echo $
    "Verifying " <> unsafeTextToLine (T.pack (show (Map.size db))) <> " packages."
  echo "Warning: this could take some time!"

  let installOrUpdate' (name, pkgInfo) = (name, ) <$> installOrUpdate (set pkg) name pkgInfo
  paths <- Map.fromList <$> traverse installOrUpdate' (Map.toList db)

  for_ (Map.toList db) $ \(name, _) -> do
    let dirFor pkgName = fromMaybe (error ("verifyPackageSet: no directory for " <> show pkgName)) (Map.lookup pkgName paths)
    echo $ "Verifying package " <> unsafeTextToLine (runPackageName name)
    dependencies <- map fst <$> getTransitiveDeps db [name]
    let srcGlobs = map (pathToTextUnsafe . (</> ("src" </> "**" </> "*.purs")) . dirFor) dependencies
    procs "purs" ("compile" : srcGlobs) empty

