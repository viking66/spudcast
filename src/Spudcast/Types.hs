{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
module Spudcast.Types where

import Control.Lens
import Data.Text (Text)
import Data.Time (UTCTime)

data AppError
  = BadPodcastId
  | DBWriteFailed
  deriving Show

newtype Env = Env { spudcastBucket :: Text }

data ReadTags = ReadTags
  { _title :: Text
  , _artist :: Text
  , _album :: Text
  , _year :: Maybe Int
  , _trackNumber :: Maybe Int
  , _genre :: Text
  , _comment :: Text
  , _duration :: Int
  }
  deriving Show
makeFieldsNoPrefix ''ReadTags

data WriteTags = WriteTags
  { _title :: Text
  , _artist :: Text
  , _album :: Text
  , _year :: Int
  , _trackNumber :: Int
  , _genre :: Text
  , _comment :: Text
  }
  deriving Show
makeFieldsNoPrefix ''WriteTags

type PodcastId = Text

data PodcastDetails = PodcastDetails
  { _createDate :: UTCTime
  , _title :: Text
  , _description :: Text
  , _link :: Text
  , _host :: Text
  , _email :: Text
  , _explicit :: Bool
  , _category :: Text
  }
  deriving (Show, Eq)
makeFieldsNoPrefix ''PodcastDetails

data Podcast = Podcast
  { _podcastId :: PodcastId
  , _podcastDetails :: PodcastDetails
  }
  deriving Show
makeFieldsNoPrefix ''Podcast

data NewEpisodeDetails = NewEpisodeDetails
  { _createDate :: UTCTime
  , _title :: Text
  , _description :: Text
  , _number :: Int
  }
  deriving (Show, Eq)
makeFieldsNoPrefix ''NewEpisodeDetails
