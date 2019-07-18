{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Spudcast.Server
  ( runServer
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson ( FromJSON
                  , ToJSON
                  )
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.Google.PubSub (PubsubMessage)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant ( Application
               , JSON
               , NoContent (..)
               , PostNoContent
               , ReqBody
               , Server
               , (:>)
               , serve
               )
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

data Req = Req
  { subscription :: Text
  , message :: PubsubMessage
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

type API = ReqBody '[JSON] Req :> PostNoContent '[JSON] NoContent

api :: Proxy API
api = Proxy

server :: Server API
server = handleMsg
  where
    handleMsg msg = liftIO (putStrLn $ show msg) >> pure NoContent

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
