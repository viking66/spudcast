{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Spudcast.Server
  ( runServer
  ) where

-- import Control.Monad.IO.Class (liftIO)
import Data.Aeson ( FromJSON
                  , ToJSON
                  , decode
                  )
import Data.ByteString.Lazy (fromStrict)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Parse (defaultParseRequestBodyOptions)
import Servant ( Application
               , Context (..)
               , JSON
               , Post
               , Server
               , (:>)
               , serveWithContext
               )
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

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

-- import Spudcast.Storage (write)

data PodcastEpisode = PodcastEpisode EpisodeDetails FilePath

data EpisodeDetails = EpisodeDetails
  { podcastName :: Text
  , host :: Text
  , genre :: Text
  , epTitle :: Text
  , epNumber :: Int
  , epDescription :: Text
  }
  deriving (Generic, FromJSON, ToJSON)

instance FromMultipart Tmp PodcastEpisode where
  fromMultipart md = PodcastEpisode
    <$> (lookupInput "episodeDetails" md >>= decode . fromStrict . encodeUtf8)
    <*> (fdPayload <$> lookupFile "audio" md)

type API = MultipartForm Tmp PodcastEpisode :> Post '[JSON] EpisodeDetails

api :: Proxy API
api = Proxy

server :: Server API
server = uploadPodcast
  where
    uploadPodcast (PodcastEpisode ed _) = pure ed
    -- uploadPodcast rambutan =
    --   let bucket = "www.stanleystots.com"
    --       input = bar rambutan
    --       output = foo rambutan
    --   in liftIO $ write bucket input output

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
