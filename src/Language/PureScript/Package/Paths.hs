{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

-- | Get the source paths for a package.

module Language.PureScript.Package.Paths (getPaths) where

import           Turtle
import           Prelude hiding (FilePath)

import Language.PureScript.Package.Types.PackageConfig (PackageConfig(..), readPackageFile)
import Language.PureScript.Package.Types.PackageInfo (PackageInfo(..))
import Language.PureScript.Package.Types.PackageName (PackageName, runPackageName)
import Language.PureScript.Package.Types.PackageSet (PackageSet, readPackageSet, getTransitiveDeps)

getPaths :: MonadIO m => m [FilePath]
getPaths = do
  pkg@PackageConfig{..} <- readPackageFile
  db <- readPackageSet pkg
  getSourcePaths pkg db depends
  where
  getSourcePaths :: MonadIO m => PackageConfig -> PackageSet -> [PackageName] -> m [FilePath]
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

