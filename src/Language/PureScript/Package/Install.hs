{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.PureScript.Package.Install (install) where

import Control.Monad.IO.Class (MonadIO)
import Data.List (nub)
import Data.Text (Text)
import Prelude hiding (FilePath)

import Language.PureScript.Package.Echo (MonadEcho(..))
import Language.PureScript.Package.Types.PackageConfig (readPackageFile, depends)
import Language.PureScript.Package.Types.PackageName (fromText)
import Language.PureScript.Package.Update (updateAndWritePackageFile)

install :: (MonadIO m, MonadEcho m) => Text -> m ()
install pkgName' = do
  pkg <- readPackageFile
  pkgName <- fromText pkgName'
  let pkg' = pkg { depends = nub (pkgName : depends pkg) } -- TODO: ordnub
  updateAndWritePackageFile pkg'
