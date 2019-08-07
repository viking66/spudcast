{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Spudcast.API
  ( API
  , AddEpisodeReq (..)
  , CreatePodcastReq (..)
  , EpisodeDetails' (..)
  , NewPodcastDetails (..)
  , PodcastResp (..)
  , api
  , mkPodcastResp
  ) where

import Data.Aeson ( FromJSON
                  , ToJSON
                  , decode
                  )
import Data.ByteString.Lazy (fromStrict)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime (..))
import GHC.Generics (Generic)
import Servant ( Capture
               , Get
               , JSON
               , PlainText
               , Post
               , (:>)
               , (:<|>) (..)
               )
import Servant.Multipart ( FromMultipart
                         , MultipartForm
                         , Tmp
                         , fdFileCType
                         , fdPayload
                         , fromMultipart
                         , lookupFile
                         , lookupInput
                         )

import Spudcast.Types

-- TODO add proper error handling and replace Maybe
type API = "ping" :> Get '[PlainText] Text
      :<|> "podcast" :> "tots" :> MultipartForm Tmp AddEpisodeReq :> Post '[PlainText] Text
      :<|> "podcast" :> Capture "podcastId" Text :> Get '[JSON] (Maybe PodcastResp)
      :<|> "podcast" :> MultipartForm Tmp CreatePodcastReq :> Post '[JSON] (Maybe PodcastResp)

api :: Proxy API
api = Proxy

data AddEpisodeReq = AddEpisodeReq
  { episodeDetails :: EpisodeDetails'
  , audioPath :: FilePath
  }

data EpisodeDetails' = EpisodeDetails'
  { podcastName :: Text
  , host :: Text
  , genre :: Text
  , epTitle :: Text
  , epNumber :: Int
  , epDescription :: Text
  }
  deriving (Generic, FromJSON, ToJSON)

instance FromMultipart Tmp AddEpisodeReq where
  fromMultipart md = AddEpisodeReq
    <$> (lookupInput "episodeDetails" md >>= decode . fromStrict . encodeUtf8)
    <*> (fdPayload <$> lookupFile "audio" md)

data CreatePodcastReq = CreatePodcastReq
  { newPodcastDetails :: NewPodcastDetails
  , imagePath :: FilePath
  , imageExt :: Text
  }

data NewPodcastDetails = NewPodcastDetails
  { title :: Text
  , description :: Text
  , link :: Text
  , host :: Text
  , email :: Text
  , explicit :: Bool
  , category :: Text
  }
  deriving (Generic, FromJSON)

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

data PodcastResp = PodcastResp
  { podcastId :: Text
  , createDate :: UTCTime
  , title :: Text
  , description :: Text
  , link :: Text
  , host :: Text
  , email :: Text
  , explicit :: Bool
  , category :: Text
  }
  deriving (Generic, ToJSON)

mkPodcastResp :: Podcast -> PodcastResp
mkPodcastResp (Podcast pId PodcastDetails{..}) = PodcastResp
  { podcastId = pId
  , createDate = createDate
  , title = title
  , description = description
  , link = link
  , host = host
  , email = email
  , explicit = explicit
  , category = category
  }
