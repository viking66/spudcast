{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Spudcast.API.Types where

import Control.Lens
import Data.Aeson ( FromJSON
                  , ToJSON
                  , decode
                  )
import Data.ByteString.Lazy (fromStrict)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime (..))
import Data.Time.Calendar (toGregorian)
import GHC.Generics (Generic)
import Servant.Multipart

import Spudcast.Types


-- Request Types --

data EpisodeDetails = EpisodeDetails
  { _podcastName :: Text
  , _host :: Text
  , _genre :: Text
  , _epTitle :: Text
  , _epNumber :: Int
  , _epDescription :: Text
  }
  deriving (Generic, FromJSON, ToJSON)
makeFieldsNoPrefix ''EpisodeDetails

data NewEpisodeReq = NewEpisodeReq
  { _episodeDetails :: EpisodeDetails
  , _audioPath :: FilePath
  }
makeFieldsNoPrefix ''NewEpisodeReq

instance FromMultipart Tmp NewEpisodeReq where
  fromMultipart md = NewEpisodeReq
    <$> (lookupInput "episodeDetails" md >>= decode . fromStrict . encodeUtf8)
    <*> (fdPayload <$> lookupFile "audio" md)

data NewPodcastDetails = NewPodcastDetails
  { _title :: Text
  , _description :: Text
  , _link :: Text
  , _host :: Text
  , _email :: Text
  , _explicit :: Bool
  , _category :: Text
  }
  deriving (Generic, FromJSON)
makeFieldsNoPrefix ''NewPodcastDetails

data CreatePodcastReq = CreatePodcastReq
  { _newPodcastDetails :: NewPodcastDetails
  , _imagePath :: FilePath
  , _imageExt :: Text
  }
makeFieldsNoPrefix ''CreatePodcastReq

fileTypeExt :: Text -> Maybe Text
fileTypeExt = getExt . fileTypeParts
  where
    fileTypeParts = fmap (Text.drop 1) . Text.span (/= '/')
    getExt (_, "") = Nothing
    getExt ("image", ext) = Just ext
    getExt _ = Nothing

instance FromMultipart Tmp CreatePodcastReq where
  fromMultipart md = CreatePodcastReq
    <$> (lookupInput "newPodcastDetails" md >>= decode . fromStrict . encodeUtf8)
    <*> (fdPayload <$> mdImage)
    <*> (mdImage >>= fileTypeExt . fdFileCType)
      where mdImage = lookupFile "image" md

-- Response Types

data PodcastResp = PodcastResp
  { _podcastId :: Text
  , _createDate :: UTCTime
  , _title :: Text
  , _description :: Text
  , _link :: Text
  , _host :: Text
  , _email :: Text
  , _explicit :: Bool
  , _category :: Text
  }
  deriving (Generic, ToJSON)
makeFieldsNoPrefix ''PodcastResp

-- Type Conversions

podcastToResp :: Podcast -> PodcastResp
podcastToResp p =
  let pd = p^.podcastDetails
  in PodcastResp
    { _podcastId = p^.podcastId
    , _createDate = pd^.createDate
    , _title = pd^.title
    , _description = pd^.description
    , _link = pd^.link
    , _host = pd^.host
    , _email = pd^.email
    , _explicit = pd^.explicit
    , _category = pd^.category
    }

reqToWriteTags :: NewEpisodeReq -> UTCTime -> WriteTags
reqToWriteTags req t =
  let ed = req^.episodeDetails
      getYear = fromInteger . fst3 . toGregorian . utctDay
      fst3 (a,_,_) = a
  in WriteTags
    { _title = ed^.epTitle
    , _artist = ed^.host
    , _album = ed^.podcastName
    , _year = getYear t
    , _trackNumber = ed^.epNumber
    , _genre = ed^.genre
    , _comment = ed^.epDescription
    }

reqToPodcastDetails :: CreatePodcastReq -> UTCTime -> PodcastDetails
reqToPodcastDetails req t =
  let pd = req^.newPodcastDetails
  in PodcastDetails
    { _createDate = t
    , _title = pd^.title
    , _description = pd^.description
    , _link = pd^.link
    , _host = pd^.host
    , _email = pd^.email
    , _explicit = pd^.explicit
    , _category = pd^.category
    }
