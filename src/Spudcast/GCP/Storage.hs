{-# LANGUAGE FlexibleContexts #-}
module Spudcast.GCP.Storage
  ( writePrivateObject
  , writePublicPodcastObject
  ) where

import Control.Lens
import Control.Monad (void)
import Control.Monad.Reader
import Network.Google hiding (Env)
import Network.Google.Storage
import System.IO (stdout)
import Data.Text (Text)

import Spudcast.Types

writeObject :: ObjectsInsertPredefinedACL -> FilePath -> Text -> ReaderT Env IO Object
writeObject acl input output = do
  bucket <- asks spudcastBucket
  liftIO $ do
    lgr <- newLogger Debug stdout
    env <- newEnv <&> (envLogger .~ lgr) . (envScopes .~ storageReadWriteScope)
    body <- sourceBody input
    let objIns = objectsInsert bucket object'
          & oiName ?~ output
          & oiPredefinedACL ?~ acl
    runResourceT . runGoogle env $ upload objIns body

writePrivateObject :: FilePath -> Text -> ReaderT Env IO ()
writePrivateObject input output = void $ writeObject OIPAPrivate input output

-- TODO add pocdast id to the path
writePublicPodcastObject :: FilePath -> Text -> ReaderT Env IO ()
writePublicPodcastObject input output = void $ writeObject OIPAPublicRead input output
