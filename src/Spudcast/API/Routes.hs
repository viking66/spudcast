{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Spudcast.API.Routes
  ( API
  , api
  ) where

import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Servant
import Servant.Multipart ( MultipartForm
                         , Tmp
                         )

import Spudcast.API.Types

type API = "ping" :> Get '[PlainText] Text
      :<|> "podcast" :> Capture "podcastId" Text :> Get '[JSON] PodcastResp
      :<|> "podcast" :> MultipartForm Tmp CreatePodcastReq :> Post '[JSON] PodcastResp
      :<|> "podcast" :> Capture "podcastId" Text :> MultipartForm Tmp NewEpisodeReq :> Post '[PlainText] Text

api :: Proxy API
api = Proxy
