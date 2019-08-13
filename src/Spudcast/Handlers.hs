{-# LANGUAGE DuplicateRecordFields #-}
module Spudcast.Handlers
  ( createPodcast
  , getPodcast
  , pong
  , savePodcast
  , rambutan
  ) where

import Control.Lens
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime (..))
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

savePodcast :: FilePath -> WriteTags -> UTCTime -> Handler Text
savePodcast audioPath tags t = do
  u <- liftIO nextRandom
  let ufp = uploadName u t
      nfp = replaceFileName audioPath (Text.unpack ufp)
  _ <- liftIO $ renameFile audioPath nfp
  _ <- liftIO $ writeTags nfp tags
  _ <- liftIO $ Storage.writePrivateObject "www.stanleystots.com" nfp ufp
  tags <- liftIO $ readTags nfp
  size <- liftIO $ getFileSize nfp
  _ <- liftIO $ removeFile nfp
  pure $ podcastItem tags ufp u t size

getPodcast :: PodcastId -> Handler (Maybe Podcast)
getPodcast p = liftIO $ DB.getPodcast p

createPodcast :: FilePath -> Text -> PodcastDetails -> Handler (Maybe Podcast)
createPodcast imagePath imageExt pd = do
  let imagePath' = imagePath <> "." <> Text.unpack imageExt
  _ <- liftIO $ renameFile imagePath imagePath'
  mPodcast <- liftIO $ DB.createPodcast pd
  _ <- maybe (pure ()) (writeImage imagePath' imageExt . (^.podcastId)) mPodcast
  pure mPodcast

writeImage :: FilePath -> Text -> PodcastId -> Handler ()
writeImage imagePath imageExt podcastId =
  let storagePath = podcastId <> "/" <> "assets/cover." <> imageExt
  in liftIO $ void $
    Storage.writePublicPodcastObject imagePath storagePath

rambutan :: PodcastId -> Handler Text
rambutan p = do
  mPodcast <- liftIO $ DB.getPodcast p
  pure $ maybe "No podcast found" (Text.pack . show) mPodcast
