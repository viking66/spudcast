{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Spudcast.API
  ( API
  , EpisodeDetails (..)
  , PodcastEpisode
  , PodcastResponse
  , api
  , getAudioPath
  , getEpisodeDetails
  , toPodcastResponse
  ) where

import Data.Aeson ( FromJSON
                  , ToJSON
                  , decode
                  )
import Data.ByteString.Lazy (fromStrict)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Servant ( Capture
               , Get
               , JSON
               , PlainText
               , Post
               , (:>)
               , (:<|>)
               )
import Servant.Multipart ( FromMultipart
                         , Mem
                         , MultipartForm
                         , Tmp
                         , fdPayload
                         , fromMultipart
                         , lookupFile
                         , lookupInput
                         )

import Spudcast.Types

-- TODO add proper error handling and replace Maybe
type API = "ping" :> Get '[JSON] Text
      :<|> "podcast" :> "tots" :> MultipartForm Tmp PodcastEpisode :> Post '[PlainText] Text
      :<|> "podcast" :> Capture "podcastId" Text :> Get '[JSON] (Maybe PodcastResponse)
      :<|> "podcast" :> MultipartForm Mem CreatePodcast :> Post '[JSON] PodcastResponse

api :: Proxy API
api = Proxy

data PodcastEpisode = PodcastEpisode EpisodeDetails FilePath

data EpisodeDetails = EpisodeDetails
  { podcastName :: Text
  , host :: Text
  , genre :: Text
  , epTitle :: Text
  , epNumber :: Int
  , epDescription :: Text
  }
  deriving (Generic, FromJSON, ToJSON)

instance FromMultipart Tmp PodcastEpisode where
  fromMultipart md = PodcastEpisode
    <$> (lookupInput "episodeDetails" md >>= decode . fromStrict . encodeUtf8)
    <*> (fdPayload <$> lookupFile "audio" md)

getEpisodeDetails :: PodcastEpisode -> EpisodeDetails
getEpisodeDetails (PodcastEpisode ed _) = ed

getAudioPath :: PodcastEpisode -> FilePath
getAudioPath (PodcastEpisode _ fp) = fp

data PodcastResponse = PodcastResponse
  { podcastId :: Text
  , createDate :: UTCTime
  , title :: Text
  , description :: Text
  , link :: Text
  , host :: Text
  , email :: Text
  , explicit :: Bool
  , category :: Text
  , imageUrl :: Text
  }
  deriving (Show, Generic, ToJSON)

toPodcastResponse :: PodcastDetails -> PodcastResponse
toPodcastResponse PodcastDetails{..} = PodcastResponse
  { podcastId = unPodcastId pId
  , createDate = unPodcastCreateDate createDate
  , title = unPodcastTitle title
  , description = unPodcastDescription description
  , link = unPodcastLink link
  , host = unPodcastHost host
  , email = unPodcastEmail email
  , explicit = unPodcastExplicit explicit
  , category = unPodcastCategory category
  , imageUrl = unPodcastImageUrl imageUrl
  }
