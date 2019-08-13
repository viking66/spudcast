{-# LANGUAGE DataKinds #-}

module Spudcast.Server
  ( runServer
  ) where

import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import Spudcast.API
import qualified Spudcast.Handlers as Handlers

server :: Server API
server = Handlers.pong
    :<|> getPodcast
    :<|> createPodcast
    :<|> writeEpisode

  where
    getPodcast :: Text -> Handler (Maybe PodcastResp)
    getPodcast = fmap (podcastToResp <$>) . Handlers.getPodcast

    createPodcast :: CreatePodcastReq -> Handler (Maybe PodcastResp)
    createPodcast req = do
      t <- liftIO getCurrentTime
      mp <- Handlers.createPodcast
        (req^.imagePath)
        (req^.imageExt)
        (reqToPodcastDetails req t)
      pure $ podcastToResp <$> mp

    writeEpisode :: Text -> NewEpisodeReq -> Handler Text
    writeEpisode pId req = do
      t <- liftIO getCurrentTime
      Handlers.writeEpisode pId (req^.audioPath) (reqToNewEpisodeDetails req t)

app :: Application
app = serve api server

getPort :: IO Int
getPort = do
  mPortStr <- lookupEnv "PORT"
  pure $ fromMaybe 8080 (mPortStr >>= readMaybe)

runServer :: IO ()
runServer = do
  port <- getPort
  run port $ logStdoutDev app
