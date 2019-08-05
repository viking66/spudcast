module Spudcast.Handlers
  ( getPodcast
  , pong
  , uploadPodcast
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock ( UTCTime (..)
                       , getCurrentTime
                       )
import Data.Time.Format ( defaultTimeLocale
                        , formatTime
                        )
import Data.UUID ( UUID
                 , toText
                 )
import Data.UUID.V4 (nextRandom)
import Servant.Server (Handler)
import System.Directory ( getFileSize
                        , removeFile
                        , renameFile
                        )
import System.FilePath (replaceFileName)

import Spudcast.API ( EpisodeDetails (..)
                    , PodcastResponse
                    , PodcastEpisode
                    , getAudioPath
                    , getEpisodeDetails
                    , toPodcastResponse
                    )
import qualified Spudcast.DB as DB
import Spudcast.Feed (podcastItem)
import qualified Spudcast.Storage as Storage
import Spudcast.Tags ( readTags
                     , writeTags
                     )
import Spudcast.Types ( PodcastId
                      , WriteTags (..)
                      )

pong :: Handler Text
pong = pure "pong"

getYear :: UTCTime -> Int
getYear = fromInteger . getYear' . toGregorian . utctDay
  where getYear' (y,_,_) = y

toWriteTags :: EpisodeDetails -> UTCTime -> WriteTags
toWriteTags EpisodeDetails{..} t = WriteTags
  { title = epTitle
  , artist = host
  , album = podcastName
  , year = getYear t
  , trackNumber = epNumber
  , genre = genre
  , comment = epDescription
  }

uploadName :: UUID -> UTCTime -> Text
uploadName u t = Text.intercalate "." [toText u, timePart, "mp3"]
  where timePart = Text.pack $ formatTime defaultTimeLocale "%Y%m%d%H%M" t

uploadPodcast :: PodcastEpisode -> Handler Text
uploadPodcast pe = do
  u <- liftIO nextRandom
  t <- liftIO getCurrentTime
  let ed = getEpisodeDetails pe
      ofp = getAudioPath pe
      ufp = uploadName u t
      nfp = replaceFileName ofp (Text.unpack ufp)
  _ <- liftIO $ renameFile ofp nfp
  _ <- liftIO $ writeTags nfp (toWriteTags ed t)
  _ <- liftIO $ Storage.write "www.stanleystots.com" nfp ufp
  tags <- liftIO $ readTags nfp
  size <- liftIO $ getFileSize nfp
  _ <- liftIO $ removeFile nfp
  pure $ podcastItem tags ufp u t size

getPodcast :: PodcastId -> Handler (Maybe PodcastResponse)
getPodcast p = do
  mRes <- liftIO . DB.getPodcast $ p
  pure (toPodcastResponse <$> mRes)
