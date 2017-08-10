module Language.PureScript.Package.Uninstall (uninstall) where

import Language.PureScript.Package.Types.PackageConfig (readPackageFile, depends)
import Language.PureScript.Package.Types.PackageName (packageNameFromString)
import Language.PureScript.Package.Update (updateAndWritePackageFile)

uninstall :: String -> IO ()
uninstall pkgName' = do
  pkg <- readPackageFile
  pkgName <- packageNameFromString pkgName'
  let pkg' = pkg { depends = filter (/= pkgName) $ depends pkg }
  updateAndWritePackageFile pkg'
