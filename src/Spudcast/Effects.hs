{-# LANGUAGE FlexibleInstances #-}
module Spudcast.Effects
  ( AudioTags (..)
  , DB (..)
  , Filesystem (..)
  , HasUUID (..)
  , Storage (..)
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Text (Text)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import System.Directory ( getFileSize
                        , removeFile
                        , renameFile
                        )

import Spudcast.AppM
import qualified Spudcast.AudioTags as AudioTags
import qualified Spudcast.GCP.DB as DB
import Spudcast.GCP.Storage
import Spudcast.Types

class (Monad m) => Filesystem m where
  mv :: FilePath -> FilePath -> m ()
  rm :: FilePath -> m ()
  fileSize :: FilePath -> m Integer

instance Filesystem AppM where
  mv from to = liftIO $ renameFile from to
  rm = liftIO . removeFile
  fileSize = liftIO . getFileSize

class (Monad m) => HasUUID m where
  getUUID :: m UUID

instance HasUUID AppM where
  getUUID = liftIO nextRandom

class (Monad m) => AudioTags m where
  readTags :: FilePath -> m ReadTags
  writeTags :: FilePath -> WriteTags -> m ()

instance AudioTags AppM where
  readTags = liftIO . AudioTags.readTags
  writeTags fp tags = liftIO $ AudioTags.writeTags fp tags

class (Monad m) => Storage m where
  writePrivate :: FilePath -> Text -> m ()
  writePublic :: FilePath -> Text -> m ()

instance Storage AppM where
  writePrivate input output =
    ask >>= liftIO . runReaderT (writePrivateObject input output)
  writePublic input output =
    ask >>= liftIO . runReaderT (writePublicPodcastObject input output)

class (Monad m) => DB m where
  getPodcast :: PodcastId -> m Podcast
  createPodcast :: PodcastDetails -> m Podcast

instance DB AppM where
  getPodcast p = do
    ep <- liftIO $ DB.getPodcast p
    either throwError pure ep

  createPodcast pd = do
    ep <- liftIO $ DB.createPodcast pd
    either throwError pure ep
