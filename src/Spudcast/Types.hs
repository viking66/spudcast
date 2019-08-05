{-# LANGUAGE DuplicateRecordFields #-}
module Spudcast.Types
  ( NewPodcast (..)
  , PodcastCategory (..)
  , PodcastCreateDate (..)
  , PodcastDescription (..)
  , PodcastDetails (..)
  , PodcastEmail (..)
  , PodcastExplicit (..)
  , PodcastHost (..)
  , PodcastId (..)
  , PodcastImageUrl (..)
  , PodcastLink (..)
  , PodcastTitle (..)
  , ReadTags (..)
  , WriteTags (..)
  , mkPodcastCategory
  , unPodcastCategory
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

newtype PodcastId = PodcastId { unPodcastId :: Text }
  deriving Show

newtype PodcastTitle = PodcastTitle { unPodcastTitle :: Text }
  deriving Show

newtype PodcastDescription = PodcastDescription { unPodcastDescription :: Text }
  deriving Show

newtype PodcastLink = PodcastLink { unPodcastLink :: Text }
  deriving Show

newtype PodcastHost = PodcastHost { unPodcastHost :: Text }
  deriving Show

newtype PodcastEmail = PodcastEmail { unPodcastEmail :: Text }
  deriving Show

newtype PodcastExplicit = PodcastExplicit { unPodcastExplicit :: Bool }
  deriving Show

data PodcastCategory = PodcastCategoryComedy
                     | PodcastCategoryOther Text
  deriving Show

unPodcastCategory :: PodcastCategory -> Text
unPodcastCategory PodcastCategoryComedy = "Comedy"
unPodcastCategory (PodcastCategoryOther t) = t

mkPodcastCategory :: Text -> PodcastCategory
mkPodcastCategory "Comedy" = PodcastCategoryComedy
mkPodcastCategory t = PodcastCategoryOther t

newtype PodcastImageUrl = PodcastImageUrl { unPodcastImageUrl :: Text }
  deriving Show

newtype PodcastCreateDate = PodcastCreateDate { unPodcastCreateDate :: UTCTime }
  deriving Show

data PodcastDetails = PodcastDetails
  { pId :: PodcastId
  , createDate :: PodcastCreateDate
  , title :: PodcastTitle
  , description :: PodcastDescription
  , link :: PodcastLink
  , host :: PodcastHost
  , email :: PodcastEmail
  , explicit :: PodcastExplicit
  , category :: PodcastCategory
  , imageUrl :: PodcastImageUrl
  }
  deriving Show

data NewPodcast = NewPodcast
  { title :: PodcastTitle
  , description :: PodcastDescription
  , link :: PodcastLink
  , host :: PodcastHost
  , email :: PodcastEmail
  , explicit :: PodcastExplicit
  , category :: PodcastCategory
  , imageUrl :: PodcastImageUrl
  }
  deriving Show
