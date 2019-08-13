{-# LANGUAGE DuplicateRecordFields #-}
module Spudcast.Handlers
  ( createPodcast
  , getPodcast
  , pong
  , writeEpisode
  ) where

import Control.Lens
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Calendar (toGregorian)
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

mkWriteTags :: Podcast -> NewEpisodeDetails -> UTCTime -> WriteTags
mkWriteTags p ned t =
  let getYear = fromInteger . fst3 . toGregorian . utctDay
      fst3 (a,_,_) = a
  in WriteTags
    { _title = ned^.title
    , _artist = p^.podcastDetails.host
    , _album = p^.podcastDetails.title
    , _year = getYear t
    , _trackNumber = ned^.number
    , _genre = p^.podcastDetails.category
    , _comment = ned^.description
    }

writeEpisode :: PodcastId -> FilePath -> NewEpisodeDetails -> Handler Text
writeEpisode p audioPath ned = do
  u <- liftIO nextRandom
  mPodcast <- getPodcast p
  let t = ned^.createDate
      remoteName = uploadName u t
      localName = replaceFileName audioPath (Text.unpack remoteName)
  case mPodcast of
    Nothing -> pure "Something went wrong"
    Just podcast -> do
      let tags = mkWriteTags podcast ned t
      tags' <- liftIO $ do
        _ <- renameFile audioPath localName
        _ <- writeTags localName tags
        _ <- liftIO $
          Storage.writePrivateObject "www.stanleystots.com" localName remoteName
        readTags localName
      size <- liftIO $ getFileSize localName
      _ <- liftIO $ removeFile localName
      pure $ podcastItem tags' remoteName u t size
