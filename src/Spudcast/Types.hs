{-# LANGUAGE DuplicateRecordFields #-}
module Spudcast.Types
  ( Podcast (..)
  , PodcastDetails (..)
  , PodcastId
  , ReadTags (..)
  , WriteTags (..)
  , getPodcastId
  ) where

import Data.Text (Text)
import Data.Time (UTCTime)

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

type PodcastId = Text

data PodcastDetails = PodcastDetails
  { createDate :: UTCTime
  , title :: Text
  , description :: Text
  , link :: Text
  , host :: Text
  , email :: Text
  , explicit :: Bool
  , category :: Text
  }
  deriving Show

data Podcast = Podcast PodcastId PodcastDetails
  deriving Show

getPodcastId :: Podcast -> PodcastId
getPodcastId (Podcast pId _) = pId
