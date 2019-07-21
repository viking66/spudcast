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
import Network.Wai.Parse (defaultParseRequestBodyOptions)
import Servant ( Application
               , Context (..)
               , Get
               , JSON
               , Post
               , Server
               , (:>)
               , (:<|>) (..)
               , serveWithContext
               )
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import Spudcast.Storage (listObjects)

import Servant.Multipart ( FromMultipart
                         , MultipartForm
                         , MultipartOptions (..)
                         , Tmp
                         , TmpBackendOptions (..)
                         , fdPayload
                         , fromMultipart
                         , lookupFile
                         , lookupInput
                         )
import Network.Google.Storage (Objects)

data Rambutan = Rambutan { foo :: Text, bar :: FilePath }

instance FromMultipart Tmp Rambutan where
  fromMultipart multipartData =
    Rambutan
      <$> lookupInput "foo" multipartData
      <*> fmap fdPayload (lookupFile "bar" multipartData)

type API = MultipartForm Tmp Rambutan :> Post '[JSON] Text
      :<|> Get '[JSON] Objects

api :: Proxy API
api = Proxy

server :: Server API
server = handleRambutan
    :<|> handleObjects
  where
    handleRambutan rambutan = do
      b <- liftIO $ pack <$> readFile (bar rambutan)
      pure $ foo rambutan <> b
    handleObjects = liftIO listObjects

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
