{-# LANGUAGE DataKinds #-}

module Spudcast.Server
  ( runServer
  ) where

import Data.Maybe (fromMaybe)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Parse (defaultParseRequestBodyOptions)
import Servant ( Application
               , Context (..)
               , Server
               , serveWithContext
               )
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import Servant.Multipart ( MultipartOptions (..)
                         , Tmp
                         , TmpBackendOptions (..)
                         )

import Spudcast.API ( API
                    , api
                    )
import Spudcast.Handlers (uploadPodcast)

server :: Server API
server = uploadPodcast

ctx :: Context '[MultipartOptions Tmp]
ctx = multipartOptions :. EmptyContext
  where
    multipartOptions = MultipartOptions
      { generalOptions = defaultParseRequestBodyOptions
      , backendOptions = TmpBackendOptions
        { getTmpDir = pure "."
        , filenamePat = "servant-multipart.buf"
        }
      }

app :: Application
app = serveWithContext api ctx server

getPort :: IO Int
getPort = do
  mPortStr <- lookupEnv "PORT"
  pure $ fromMaybe 8080 (mPortStr >>= readMaybe)

runServer :: IO ()
runServer = do
  port <- getPort
  run port $ logStdoutDev app
