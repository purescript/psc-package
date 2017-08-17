{-# LANGUAGE OverloadedStrings #-}

module Types
  ( PackageName
  , mkPackageName
  , runPackageName
  , preludePackageName
  , untitledPackageName
  ) where

import           Control.Category ((>>>))
import           Data.Aeson (FromJSON, ToJSON, FromJSONKey(..), ToJSONKey(..), ToJSONKeyFunction(..), FromJSONKeyFunction(..), parseJSON, toJSON, withText)
import qualified Data.Aeson.Encoding as AesonEncoding
import           Data.Char (isAscii, isLower, isDigit)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T

newtype PackageName
  = PackageName Text
  deriving (Show, Eq, Ord)

instance ToJSON PackageName where
  toJSON (PackageName t) = toJSON t

instance FromJSON PackageName where
  parseJSON =
    withText "package name" fromText

fromText :: Monad m => Text -> m PackageName
fromText t =
  case mkPackageName t of
    Right pkgName -> pure pkgName
    Left errs -> fail $ "Invalid package name: " <> show errs

instance ToJSONKey PackageName where
  toJSONKey =
    ToJSONKeyText
      runPackageName
      (AesonEncoding.text . runPackageName)

instance FromJSONKey PackageName where
  fromJSONKey =
    FromJSONKeyTextParser fromText

data PackageNameError
  = NotEmpty
  | TooLong Int
  | InvalidChars String
  | RepeatedSeparators
  | MustNotBeginSeparator
  | MustNotEndSeparator
  deriving (Show, Eq, Ord)

-- | Smart constructor for package names. Based on Bower's requirements for
-- | package names.
mkPackageName :: Text -> Either PackageNameError PackageName
mkPackageName = fmap PackageName . validateAll validators
  where
  dashOrDot = ['-', '.']
  validateAll vs x = mapM_ (validateWith x) vs >> return x
  validateWith x (p, err)
    | p x       = Right x
    | otherwise = Left (err x)
  validChar c = isAscii c && (isLower c || isDigit c || c `elem` dashOrDot)
  validators =
      [ (not . T.null, const NotEmpty)
      , (T.all validChar, InvalidChars . T.unpack . T.filter (not . validChar))
      , (firstChar (`notElem` dashOrDot), const MustNotBeginSeparator)
      , (lastChar (`notElem` dashOrDot), const MustNotEndSeparator)
      , (not . T.isInfixOf "--", const RepeatedSeparators)
      , (not . T.isInfixOf "..", const RepeatedSeparators)
      , (T.length >>> (<= 50), TooLong . T.length)
      ]
  firstChar p str = not (T.null str) && p (T.index str 0)
  lastChar p = firstChar p . T.reverse

runPackageName :: PackageName -> Text
runPackageName (PackageName t) = t

preludePackageName :: PackageName
preludePackageName = PackageName "prelude"

untitledPackageName :: PackageName
untitledPackageName = PackageName "untitled"
