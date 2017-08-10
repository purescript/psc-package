{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.PureScript.Package.Types.PackageSet (
  PackageSet
, packageSetToJSON
, getPackageSet
, readPackageSet
, writePackageSet
) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonEncode
import           Data.Map (Map)
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import           Turtle
import           Prelude hiding (FilePath)

import Language.PureScript.Package.Types.PackageConfig (PackageConfig(..))
import Language.PureScript.Package.Types.PackageInfo (PackageInfo)
import Language.PureScript.Package.Types.PackageName (PackageName)
import Language.PureScript.Package.Git (cloneShallow)

type PackageSet = Map PackageName PackageInfo

packageSetToJSON :: PackageSet -> Text
packageSetToJSON =
    TL.toStrict
    . TB.toLazyText
    . AesonEncode.encodePrettyToTextBuilder' config
  where
    config = AesonEncode.defConfig { AesonEncode.confCompare = compare }

getPackageSet :: PackageConfig -> IO ()
getPackageSet PackageConfig{ source, set } = do
  let pkgDir = ".psc-package" </> fromText set </> ".set"
  exists <- testdir pkgDir
  unless exists . void $ cloneShallow source set pkgDir

readPackageSet :: PackageConfig -> IO PackageSet
readPackageSet PackageConfig{ set } = do
  let dbFile = ".psc-package" </> fromText set </> ".set" </> "packages.json"
  exists <- testfile dbFile
  unless exists $ die $ format (fp%" does not exist") dbFile
  mdb <- Aeson.decodeStrict . encodeUtf8 <$> readTextFile dbFile
  case mdb of
    Nothing -> die "Unable to parse packages.json"
    Just db -> return db

writePackageSet :: PackageConfig -> PackageSet -> IO ()
writePackageSet PackageConfig{ set } =
  let dbFile = ".psc-package" </> fromText set </> ".set" </> "packages.json"
  in writeTextFile dbFile . packageSetToJSON
