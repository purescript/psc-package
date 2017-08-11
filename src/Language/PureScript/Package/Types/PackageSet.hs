{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.PureScript.Package.Types.PackageSet (
  PackageSet
, packageSetToJSON
, getPackageSet
, readPackageSet
, writePackageSet
, getTransitiveDeps
) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonEncode
import           Data.Foldable (fold)
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import           Turtle hiding (fold)
import           Prelude hiding (FilePath)

import Language.PureScript.Package.Types.PackageConfig (PackageConfig(..))
import Language.PureScript.Package.Types.PackageInfo (PackageInfo(..))
import Language.PureScript.Package.Types.PackageName (PackageName, runPackageName)
import Language.PureScript.Package.Git (cloneShallow)

type PackageSet = Map PackageName PackageInfo

packageSetToJSON :: PackageSet -> Text
packageSetToJSON =
    TL.toStrict
    . TB.toLazyText
    . AesonEncode.encodePrettyToTextBuilder' config
  where
    config = AesonEncode.defConfig { AesonEncode.confCompare = compare }

getPackageSet :: MonadIO m => PackageConfig -> m ()
getPackageSet PackageConfig{ source, set } = do
  let pkgDir = ".psc-package" </> fromText set </> ".set"
  exists <- testdir pkgDir
  unless exists . void $ cloneShallow source set pkgDir

readPackageSet :: MonadIO m => PackageConfig -> m PackageSet
readPackageSet PackageConfig{ set } = do
  let dbFile = ".psc-package" </> fromText set </> ".set" </> "packages.json"
  exists <- testfile dbFile
  unless exists $ die $ format (fp%" does not exist") dbFile
  mdb <- liftIO $ Aeson.decodeStrict . encodeUtf8 <$> readTextFile dbFile
  case mdb of
    Nothing -> die "Unable to parse packages.json"
    Just db -> return db

writePackageSet :: PackageConfig -> PackageSet -> IO ()
writePackageSet PackageConfig{ set } =
  let dbFile = ".psc-package" </> fromText set </> ".set" </> "packages.json"
  in writeTextFile dbFile . packageSetToJSON

getTransitiveDeps
  :: MonadIO m
  => PackageSet
  -> [PackageName]
  -> m [(PackageName, PackageInfo)]
getTransitiveDeps db deps =
    Map.toList . fold <$> traverse (go Set.empty) deps
  where
    go seen pkg
      | pkg `Set.member` seen =
          die $ "Cycle in package dependencies at package " <> runPackageName pkg
      | otherwise =
        case Map.lookup pkg db of
          Nothing ->
            die $ "Package " <> runPackageName pkg <> " does not exist in package set"
          Just info@PackageInfo{ dependencies } -> do
            m <- fold <$> traverse (go (Set.insert pkg seen)) dependencies
            return (Map.insert pkg info m)

