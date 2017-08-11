{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Language.PureScript.Package.Types.PackageConfig (
  PackageConfig(..)
, readPackageFile
, writePackageFile
) where

import           Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonEncode
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import           GHC.Generics (Generic)
import           Turtle
import           Prelude hiding (FilePath)

import           Language.PureScript.Package.Types.PackageName (PackageName)

data PackageConfig = PackageConfig
  { name    :: PackageName
  , set     :: Text
  , source  :: Text
  , depends :: [PackageName]
  } deriving (Show, Generic, FromJSON, ToJSON)

packageConfigToJSON :: PackageConfig -> Text
packageConfigToJSON =
    TL.toStrict
    . TB.toLazyText
    . AesonEncode.encodePrettyToTextBuilder' config
  where
    config = AesonEncode.defConfig
               { AesonEncode.confCompare =
                   AesonEncode.keyOrder [ "name"
                                        , "set"
                                        , "source"
                                        , "depends"
                                        ]
               }

packageFile :: FilePath
packageFile = "psc-package.json"

readPackageFile :: MonadIO m => m PackageConfig
readPackageFile = do
  exists <- testfile packageFile
  unless exists $ die "psc-package.json does not exist. Maybe you need to run psc-package init?"
  mpkg <- liftIO $ Aeson.decodeStrict . encodeUtf8 <$> readTextFile packageFile
  case mpkg of
    Nothing -> die "Unable to parse psc-package.json"
    Just pkg -> return pkg

writePackageFile :: MonadIO m => PackageConfig -> m ()
writePackageFile = liftIO . writeTextFile packageFile . packageConfigToJSON
