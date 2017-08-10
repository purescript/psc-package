{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.PureScript.Package.Update (updateImpl) where

import           Control.Concurrent.Async (forConcurrently_)
import qualified Data.Text as T
import           Turtle
import           Prelude hiding (FilePath)

import Language.PureScript.Package.Types.PackageConfig (PackageConfig(..))
import Language.PureScript.Package.Types.PackageSet (readPackageSet, getTransitiveDeps)
import Language.PureScript.Package.Install (installOrUpdate)
import Language.PureScript.Package.Git (cloneShallow)

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
