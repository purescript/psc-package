{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Update (obtain) dependencies/packages.

module Language.PureScript.Package.Update (
  update
, updateImpl
, updateAndWritePackageFile
, installOrUpdate
) where

import           Control.Concurrent.Async (forConcurrently_)
import qualified Data.Text as T
import           Turtle hiding (echo)
import           Prelude hiding (FilePath)

import Language.PureScript.Package.Echo (MonadEcho(..))
import Language.PureScript.Package.Git (cloneShallow)
import Language.PureScript.Package.Types.PackageConfig (PackageConfig(..), readPackageFile, writePackageFile)
import Language.PureScript.Package.Types.PackageInfo (PackageInfo(..))
import Language.PureScript.Package.Types.PackageName (PackageName, runPackageName)
import Language.PureScript.Package.Types.PackageSet (readPackageSet, getTransitiveDeps)

update :: (MonadIO m, MonadEcho m) => m ()
update = do
  pkg <- readPackageFile
  updateImpl pkg
  echo "Update complete"

updateImpl :: (MonadIO m, MonadEcho m) => PackageConfig -> m ()
updateImpl config@PackageConfig{ depends } = do
  getPackageSet config
  db <- readPackageSet config
  trans <- getTransitiveDeps db depends
  echo $ "Updating " <> T.pack (show (length trans)) <> " packages..."
  liftIO . forConcurrently_ trans . uncurry $ installOrUpdate (set config)

  where
  getPackageSet :: MonadIO m => PackageConfig -> m ()
  getPackageSet PackageConfig{ source, set } = do
    let pkgDir = ".psc-package" </> fromText set </> ".set"
    exists <- testdir pkgDir
    unless exists . void $ cloneShallow source set pkgDir

updateAndWritePackageFile :: (MonadIO m, MonadEcho m) => PackageConfig -> m ()
updateAndWritePackageFile pkg = do
  updateImpl pkg
  writePackageFile pkg
  echo "psc-package.json file was updated"

installOrUpdate :: (MonadIO m, MonadEcho m) => Text -> PackageName -> PackageInfo -> m FilePath
installOrUpdate set pkgName PackageInfo{ repo, version } = do
  let pkgDir = ".psc-package" </> fromText set </> fromText (runPackageName pkgName) </> fromText version
  exists <- testdir pkgDir
  unless exists . void $ do
    echo $ "Updating " <> runPackageName pkgName
    cloneShallow repo version pkgDir
  pure pkgDir
