{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Spudcast.Server
  ( runServer
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Text ( Text
                 , pack
                 )
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant ( Application
               , JSON
               , Post
               , Server
               , (:>)
               , serve
               )
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import Servant.Multipart
import Network.Wai.Parse

data Rambutan = Rambutan { foo :: Text, bar :: FilePath }

data Tmp'

instance MultipartBackend Tmp' where
    type MultipartResult Tmp' = FilePath
    type MultipartBackendOptions Tmp' = TmpBackendOptions

    defaultBackendOptions _ = TmpBackendOptions
      { getTmpDir = pure "."
      , filenamePat = "servant-multipart.buf"
      }
    backend _ opts = tmpBackend
      where
        tmpBackend = tempFileBackEndOpts (getTmpDir opts) (filenamePat opts)

instance FromMultipart Tmp' Rambutan where
  fromMultipart multipartData =
    Rambutan
      <$> lookupInput "foo" multipartData
      <*> fmap fdPayload (lookupFile "bar" multipartData)

type API = MultipartForm Tmp' Rambutan :> Post '[JSON] Text

api :: Proxy API
api = Proxy

server :: Server API
server = handleRambutan
  where
    handleRambutan rambutan = do
      b <- liftIO $ pack <$> readFile (bar rambutan)
      pure $ foo rambutan <> b

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
