module Spudcast.DB
  ( PodcastDocument (..)
  , getPodcastDocument
  ) where

import Control.Lens
import Data.Text (Text)
import Data.Time (UTCTime)
import Network.Google ( LogLevel (..)
                      , envLogger
                      , envScopes
                      , newEnv
                      , newLogger
                      , runGoogle
                      , runResourceT
                      , send
                      )
import Network.Google.Prelude (HashMap)
import Network.Google.FireStore ( Document
                                , Value
                                , dFields
                                , datastoreScope
                                , dfAddtional
                                , projectsDatabasesDocumentsGet
                                , vStringValue
                                , vTimestampValue
                                )
import System.IO (stdout)

-- TODO move this into Types
-- TODO add id to record
data PodcastDocument = PodcastDocument
  { podcastRoot :: Text
  , podcastCreateDate :: UTCTime
  , podcastGenre :: Text
  , podcastHost :: Text
  , podcastName :: Text
  }
  deriving Show

getDocument :: Text -> IO Document
getDocument d = do
  lgr <- newLogger Debug stdout
  env <- newEnv <&> (envLogger .~ lgr) . (envScopes .~ datastoreScope)
  let req = "projects/stanleystots/databases/(default)/documents/" <> d
  runResourceT . runGoogle env . send . projectsDatabasesDocumentsGet $ req

getTextValue :: HashMap Text Value -> Text -> Maybe Text
getTextValue m k = (m ^@? iix k)^?(_Just._2) >>= view vStringValue

getTimestampValue :: HashMap Text Value -> Text -> Maybe UTCTime
getTimestampValue m k = (m ^@? iix k)^?(_Just._2) >>= view vTimestampValue

-- getPodcastDocument "podcasts/d0HLU6SHlnKlHeuV1DAB"
getPodcastDocument :: Text -> IO (Maybe PodcastDocument)
getPodcastDocument = fmap toPodcastDocument . getDocument
  where
    toPodcastDocument d = getDocMap d >>= fromMap
    fromMap m = PodcastDocument
      <$> getTextValue m "bucket"
      <*> getTimestampValue m "createDate"
      <*> getTextValue m "genre"
      <*> getTextValue m "host"
      <*> getTextValue m "name"
    getDocMap doc = doc^.dFields^?(_Just.dfAddtional)
