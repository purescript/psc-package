{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Language.PureScript.Package.Types.PackageConfig (PackageConfig(..)) where

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Text (Text)
import           GHC.Generics (Generic)

import           Language.PureScript.Package.Types.PackageName (PackageName)

data PackageConfig = PackageConfig
  { name    :: PackageName
  , depends :: [PackageName]
  , set     :: Text
  , source  :: Text
  } deriving (Show, Generic, FromJSON, ToJSON)
