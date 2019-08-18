{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
module Spudcast.Handlers
  ( createPodcast
  , getPodcast
  , writeEpisode
  ) where

import Control.Lens
import Control.Monad (void)
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
import System.FilePath (replaceFileName)

import Spudcast.Effects hiding ( createPodcast
                               , getPodcast
                               )
import qualified Spudcast.Effects as Effects
import Spudcast.Feed
import Spudcast.Types

uploadName :: UUID -> UTCTime -> Text
uploadName u t = Text.intercalate "." [toText u, timePart, "mp3"]
  where timePart = Text.pack $ formatTime defaultTimeLocale "%Y%m%d%H%M" t

getPodcast :: (DB m) => PodcastId -> m Podcast
getPodcast = Effects.getPodcast

createPodcast :: (Filesystem m, Storage m, DB m)
              => FilePath
              -> Text
              -> PodcastDetails
              -> m Podcast
createPodcast imagePath imageExt pd = do
  let imagePath' = imagePath <> "." <> Text.unpack imageExt
  _ <- mv imagePath imagePath'
  podcast <- Effects.createPodcast pd
  _ <- writeImage imagePath' imageExt (podcast^.podcastId)
  pure podcast

writeImage :: (Storage m) => FilePath -> Text -> PodcastId -> m ()
writeImage imagePath imageExt podcastId =
  let storagePath = podcastId <> "/" <> "assets/cover." <> imageExt
  in void $ writePublic imagePath storagePath

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

writeEpisode :: (Filesystem m, HasUUID m, Storage m, AudioTags m, DB m)
              => PodcastId
              -> FilePath
              -> NewEpisodeDetails
              -> m Text
writeEpisode p audioPath ned = do
  u <- getUUID
  podcast <- Effects.getPodcast p
  let t = ned^.createDate
      remoteName = uploadName u t
      localName = replaceFileName audioPath (Text.unpack remoteName)
      tags = mkWriteTags podcast ned t
  _ <- mv audioPath localName
  _ <- writeTags localName tags
  tags' <- readTags localName
  size <- fileSize localName
  _ <- writePrivate localName remoteName
  _ <- rm localName
  pure $ podcastItem tags' remoteName u t size
