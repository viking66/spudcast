{-# LANGUAGE DuplicateRecordFields #-}
module Spudcast.Types
  ( ReadTags (..)
  , WriteTags (..)
  ) where

import Data.Text (Text)

data ReadTags = ReadTags
  { title :: Text
  , artist :: Text
  , album :: Text
  , year :: Maybe Int
  , trackNumber :: Maybe Int
  , genre :: Text
  , comment :: Text
  , duration :: Int
  }
  deriving Show

data WriteTags = WriteTags
  { title :: Text
  , artist :: Text
  , album :: Text
  , year :: Int
  , trackNumber :: Int
  , genre :: Text
  , comment :: Text
  }
  deriving Show
