{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.PureScript.Package.Install (install) where

import           Data.List (nub)
import           Prelude hiding (FilePath)

import Language.PureScript.Package.Types.PackageConfig (readPackageFile, depends)
import Language.PureScript.Package.Types.PackageName (packageNameFromString)
import Language.PureScript.Package.Update (updateAndWritePackageFile)

install :: String -> IO ()
install pkgName' = do
  pkg <- readPackageFile
  pkgName <- packageNameFromString pkgName'
  let pkg' = pkg { depends = nub (pkgName : depends pkg) } -- TODO: ordnub
  updateAndWritePackageFile pkg'
