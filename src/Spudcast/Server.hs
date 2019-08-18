{-# LANGUAGE DataKinds #-}

module Spudcast.Server
  ( runServer
  ) where

import Control.Lens
import Control.Monad.Error.Class (liftEither)
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
import Spudcast.AppM
import qualified Spudcast.Handlers as Handlers
import Spudcast.Types

server :: ServerT API AppM
server = pure "pong"
    :<|> getPodcast
    :<|> createPodcast
    :<|> writeEpisode

  where
    getPodcast :: Text -> AppM PodcastResp
    getPodcast = fmap podcastToResp . Handlers.getPodcast

    createPodcast :: CreatePodcastReq -> AppM PodcastResp
    createPodcast req = do
      t <- liftIO getCurrentTime
      ep <- Handlers.createPodcast
        (req^.imagePath)
        (req^.imageExt)
        (reqToPodcastDetails req t)
      pure $ podcastToResp ep

    writeEpisode :: Text -> NewEpisodeReq -> AppM Text
    writeEpisode pId req = do
      t <- liftIO getCurrentTime
      Handlers.writeEpisode
        pId
        (req^.audioPath)
        (reqToNewEpisodeDetails req t)

toServerError :: AppError -> ServantErr
toServerError BadPodcastId =
  err400 { errBody = "No podcast found for given id." }
toServerError DBWriteFailed =
  err400 { errBody = "Something went wrong with the database." }

toHandler :: Env -> AppM a -> Handler a
toHandler e a =
  liftEither =<< liftIO (over _Left toServerError <$> runAppM a e)

app :: Env -> Application
app e = serve api $ hoistServer api (toHandler e) server

getPort :: IO Int
getPort = do
  mPortStr <- lookupEnv "PORT"
  pure $ fromMaybe 8080 (mPortStr >>= readMaybe)

runServer :: IO ()
runServer = do
  port <- getPort
  run port $ logStdoutDev $ app $ Env "spudcast_dev"
