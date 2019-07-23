{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Spudcast.API
  ( API
  , EpisodeDetails (..)
  , PodcastEpisode
  , api
  , getAudioPath
  , getEpisodeDetails
  ) where

import Data.Aeson ( FromJSON
                  , ToJSON
                  , decode
                  )
import Data.ByteString.Lazy (fromStrict)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import Servant ( Get
               , JSON
               , PlainText
               , Post
               , (:>)
               , (:<|>)
               )

import Servant.Multipart ( FromMultipart
                         , MultipartForm
                         , Tmp
                         , fdPayload
                         , fromMultipart
                         , lookupFile
                         , lookupInput
                         )

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

type API = "ping" :> Get '[JSON] Text
      :<|> "podcast" :> MultipartForm Tmp PodcastEpisode :> Post '[PlainText] Text

api :: Proxy API
api = Proxy
