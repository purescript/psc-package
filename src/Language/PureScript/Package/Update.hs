{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.PureScript.Package.Update (
  update
, updateImpl
, updateAndWritePackageFile
, installOrUpdate
) where

import           Control.Concurrent.Async (forConcurrently_)
import qualified Data.Text as T
import           Turtle
import           Prelude hiding (FilePath)

import Language.PureScript.Package.Types.PackageConfig (PackageConfig(..), readPackageFile, writePackageFile)
import Language.PureScript.Package.Types.PackageInfo (PackageInfo(..))
import Language.PureScript.Package.Types.PackageName (PackageName, runPackageName)
import Language.PureScript.Package.Types.PackageSet (readPackageSet, getTransitiveDeps)
import Language.PureScript.Package.Git (cloneShallow)

update :: IO ()
update = do
  pkg <- readPackageFile
  updateImpl pkg
  echo "Update complete"

updateImpl :: PackageConfig -> IO ()
updateImpl config@PackageConfig{ depends } = do
  getPackageSet config
  db <- readPackageSet config
  trans <- getTransitiveDeps db depends
  echo ("Updating " <> unsafeTextToLine (T.pack (show (length trans))) <> " packages...")
  forConcurrently_ trans . uncurry $ installOrUpdate (set config)

  where
  getPackageSet :: PackageConfig -> IO ()
  getPackageSet PackageConfig{ source, set } = do
    let pkgDir = ".psc-package" </> fromText set </> ".set"
    exists <- testdir pkgDir
    unless exists . void $ cloneShallow source set pkgDir

updateAndWritePackageFile :: PackageConfig -> IO ()
updateAndWritePackageFile pkg = do
  updateImpl pkg
  writePackageFile pkg
  echo "psc-package.json file was updated"

installOrUpdate :: Text -> PackageName -> PackageInfo -> IO FilePath
installOrUpdate set pkgName PackageInfo{ repo, version } = do
  let pkgDir = ".psc-package" </> fromText set </> fromText (runPackageName pkgName) </> fromText version
  exists <- testdir pkgDir
  unless exists . void $ do
    echo $ "Updating " <> unsafeTextToLine (runPackageName pkgName)
    cloneShallow repo version pkgDir
  pure pkgDir
