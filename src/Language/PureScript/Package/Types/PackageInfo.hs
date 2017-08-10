{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Language.PureScript.Package.Types.PackageInfo (PackageInfo(..)) where

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Text (Text)
import           GHC.Generics (Generic)

import           Language.PureScript.Package.Types.PackageName (PackageName)

data PackageInfo = PackageInfo
  { repo         :: Text
  , version      :: Text
  , dependencies :: [PackageName]
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)
