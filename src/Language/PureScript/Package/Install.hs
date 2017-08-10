{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.PureScript.Package.Install (installOrUpdate) where

import           Turtle
import           Prelude hiding (FilePath)

import Language.PureScript.Package.Types.PackageInfo (PackageInfo(..))
import Language.PureScript.Package.Types.PackageName (PackageName, runPackageName)
import Language.PureScript.Package.Git (cloneShallow)

installOrUpdate :: Text -> PackageName -> PackageInfo -> IO FilePath
installOrUpdate set pkgName PackageInfo{ repo, version } = do
  let pkgDir = ".psc-package" </> fromText set </> fromText (runPackageName pkgName) </> fromText version
  exists <- testdir pkgDir
  unless exists . void $ do
    echo $ "Updating " <> unsafeTextToLine (runPackageName pkgName)
    cloneShallow repo version pkgDir
  pure pkgDir
