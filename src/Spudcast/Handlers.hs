{-# LANGUAGE DuplicateRecordFields #-}
module Spudcast.Handlers
  ( createPodcast
  , getPodcast
  , pong
  , uploadPodcast
  ) where

import Control.Monad (void)
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

import Spudcast.API
import qualified Spudcast.DB as DB
import Spudcast.Feed (podcastItem)
import qualified Spudcast.Storage as Storage
import Spudcast.Tags ( readTags
                     , writeTags
                     )
import Spudcast.Types

pong :: Handler Text
pong = pure "pong"

uploadName :: UUID -> UTCTime -> Text
uploadName u t = Text.intercalate "." [toText u, timePart, "mp3"]
  where timePart = Text.pack $ formatTime defaultTimeLocale "%Y%m%d%H%M" t

mkWriteTags :: EpisodeDetails' -> UTCTime -> WriteTags
mkWriteTags EpisodeDetails'{..} t = WriteTags
  { title = epTitle
  , artist = host
  , album = podcastName
  , year = getYear t
  , trackNumber = epNumber
  , genre = genre
  , comment = epDescription
  }
    where
      getYear = fromInteger . fst3 . toGregorian . utctDay
      fst3 (a,_,_) = a

uploadPodcast :: AddEpisodeReq -> Handler Text
uploadPodcast AddEpisodeReq{..} = do
  u <- liftIO nextRandom
  t <- liftIO getCurrentTime
  let ufp = uploadName u t
      nfp = replaceFileName audioPath (Text.unpack ufp)
  _ <- liftIO $ renameFile audioPath nfp
  _ <- liftIO $ writeTags nfp (mkWriteTags episodeDetails t)
  _ <- liftIO $ Storage.writePrivateObject "www.stanleystots.com" nfp ufp
  tags <- liftIO $ readTags nfp
  size <- liftIO $ getFileSize nfp
  _ <- liftIO $ removeFile nfp
  pure $ podcastItem tags ufp u t size

getPodcast :: PodcastId -> Handler (Maybe PodcastResp)
getPodcast p = do
  mPodcast <- liftIO $ DB.getPodcast p
  pure $ mkPodcastResp <$> mPodcast

createPodcast :: CreatePodcastReq -> Handler (Maybe PodcastResp)
createPodcast CreatePodcastReq{..}  = do
  let imagePath' = imagePath <> "." <> Text.unpack imageExt
  _ <- liftIO $ renameFile imagePath imagePath'
  mPodcast <- createPodcast' newPodcastDetails
  _ <- maybe (pure ()) (writeImage imagePath' imageExt . getPodcastId) mPodcast
  pure $ mkPodcastResp <$> mPodcast

createPodcast' :: NewPodcastDetails -> Handler (Maybe Podcast)
createPodcast' NewPodcastDetails{..} = do
  t <- liftIO getCurrentTime
  let pd = PodcastDetails
        { createDate = t
        , title = title
        , description = description
        , link = link
        , host = host
        , email = email
        , explicit = explicit
        , category = category
        }
  liftIO $ DB.createPodcast pd

writeImage :: FilePath -> Text -> PodcastId -> Handler ()
writeImage imagePath imageExt podcastId =
  let storagePath = podcastId <> "/" <> "assets/cover." <> imageExt
  in liftIO $ void $
    Storage.writePublicPodcastObject imagePath storagePath
