module Spudcast.Storage
  ( writePrivateObject
  , writePublicPodcastObject
  ) where

import Control.Lens ( (<&>)
                    , (.~)
                    , (&)
                    , (?~)
                    )
import Control.Monad (void)
import Network.Google ( LogLevel (..)
                      , envLogger
                      , envScopes
                      , newEnv
                      , newLogger
                      , runGoogle
                      , runResourceT
                      , sourceBody
                      , upload
                      )
import Network.Google.Storage ( Object
                              , ObjectsInsertPredefinedACL (..)
                              , object'
                              , objectsInsert
                              , oiName
                              , oiPredefinedACL
                              , storageReadWriteScope
                              )
import System.IO (stdout)
import Data.Text (Text)

spudcastBucket :: Text
spudcastBucket = "spudcast_dev"

writePrivateObject :: Text -> FilePath -> Text -> IO Object
writePrivateObject = writeObject OIPAPrivate

writeObject :: ObjectsInsertPredefinedACL -> Text -> FilePath -> Text -> IO Object
writeObject acl bucket input output = do
  lgr <- newLogger Debug stdout
  env <- newEnv <&> (envLogger .~ lgr) . (envScopes .~ storageReadWriteScope)
  body <- sourceBody input
  let objIns = objectsInsert bucket object'
        & oiName ?~ output
        & oiPredefinedACL ?~ acl
  runResourceT . runGoogle env $ upload objIns body

-- TODO make more generic
-- Pass bucket via reader
-- Rename function
writePublicPodcastObject :: FilePath -> Text -> IO ()
writePublicPodcastObject input output =
  void $ writeObject OIPAPublicRead spudcastBucket input output
