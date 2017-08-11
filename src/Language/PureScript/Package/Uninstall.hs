module Language.PureScript.Package.Uninstall (uninstall) where

import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)

import Language.PureScript.Package.Echo (MonadEcho(..))
import Language.PureScript.Package.Types.PackageConfig (readPackageFile, depends)
import Language.PureScript.Package.Types.PackageName (fromText)
import Language.PureScript.Package.Update (updateAndWritePackageFile)

uninstall :: (MonadIO m, MonadEcho m) => Text -> m ()
uninstall pkgName' = do
  pkg <- readPackageFile
  pkgName <- fromText pkgName'
  let pkg' = pkg { depends = filter (/= pkgName) $ depends pkg }
  updateAndWritePackageFile pkg'
