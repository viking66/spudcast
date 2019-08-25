{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Spudcast.API.Types
  ( CreatePodcastReq (..)
  , EpisodeDetails (..)
  , NewEpisodeReq (..)
  , NewPodcastDetails (..)
  , PodcastResp (..)
  , audioPath
  , episodeDetails
  , imageExt
  , imagePath
  , newEpIso
  , podcastToResp
  , reqToPodcastDetails
  ) where

import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import Data.ByteString.Lazy (fromStrict)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime (..))
import GHC.Generics (Generic)
import Servant.Multipart

import Spudcast.Types


-- Request Types --

data NewPodcastDetails = NewPodcastDetails
  { _title :: Text
  , _description :: Text
  , _link :: Text
  , _host :: Text
  , _email :: Text
  , _explicit :: Bool
  , _category :: Text
  }
  deriving (Show, Eq, Generic)
makeFieldsNoPrefix ''NewPodcastDetails
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''NewPodcastDetails

data CreatePodcastReq = CreatePodcastReq
  { _newPodcastDetails :: NewPodcastDetails
  , _imagePath :: FilePath
  , _imageExt :: Text
  }
  deriving (Show, Eq)
makeFieldsNoPrefix ''CreatePodcastReq

-- Expects a filetype of the form type/subtype (e.g. image/jpeg)
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

-- TODO remove number and query db instead
data EpisodeDetails = EpisodeDetails
  { _title :: Text
  , _description :: Text
  , _number :: Int
  }
  deriving (Show, Eq, Generic)
makeFieldsNoPrefix ''EpisodeDetails
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''EpisodeDetails

data NewEpisodeReq = NewEpisodeReq
  { _episodeDetails :: EpisodeDetails
  , _audioPath :: FilePath
  }
  deriving (Show, Eq)
makeFieldsNoPrefix ''NewEpisodeReq

instance FromMultipart Tmp NewEpisodeReq where
  fromMultipart md = NewEpisodeReq
    <$> (lookupInput "episodeDetails" md >>= decode . fromStrict . encodeUtf8)
    <*> (fdPayload <$> lookupFile "audio" md)

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
  deriving (Show, Eq, Generic)
makeFieldsNoPrefix ''PodcastResp
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''PodcastResp

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

newEpIso :: Iso' (EpisodeDetails, UTCTime) NewEpisodeDetails
newEpIso = iso fromReq toReq
  where
    fromReq :: (EpisodeDetails, UTCTime) -> NewEpisodeDetails
    fromReq (ed, t) = NewEpisodeDetails
      { _createDate = t
      , _title = ed^.title
      , _description = ed^.description
      , _number = ed^.number
      }
    toReq :: NewEpisodeDetails -> (EpisodeDetails, UTCTime)
    toReq ned = (ed, ned^.createDate)
      where
        ed = EpisodeDetails
          { _title = ned^.title
          , _description = ned^.description
          , _number = ned^.number
          }
