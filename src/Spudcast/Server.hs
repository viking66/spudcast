{-# LANGUAGE DataKinds #-}

module Spudcast.Server
  ( runServer
  ) where

import Data.Maybe (fromMaybe)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant ( Application
               , (:<|>) (..)
               , Server
               , serve
               )
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import Spudcast.API ( API
                    , api
                    )
import Spudcast.Handlers ( createPodcast
                         , getPodcast
                         , pong
                         , uploadPodcast
                         )

server :: Server API
server = pong
    :<|> uploadPodcast
    :<|> getPodcast
    :<|> createPodcast

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
