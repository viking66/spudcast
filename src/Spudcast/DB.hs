module Spudcast.DB
  ( PodcastDocument (..)
  , createPodcast
  , getPodcast
  ) where

import Control.Lens
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time ( UTCTime
                 , getCurrentTime
                 )
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
                                , dCreateTime
                                , dName
                                , dUpdateTime
                                , datastoreScope
                                , dfAddtional
                                , document
                                , documentFields
                                , projectsDatabasesDocumentsCreateDocument
                                , projectsDatabasesDocumentsGet
                                , vStringValue
                                , value
                                )
import System.IO (stdout)

-- TODO move this into Types
data PodcastDocument = PodcastDocument
  { podcastId :: Text
  , podcastCreateDate :: UTCTime
  , podcastGenre :: Text
  , podcastHost :: Text
  , podcastName :: Text
  }
  deriving Show

newtype Root = Root { unRoot :: Text }
newtype Collection = Collection { unCollection :: Text }
newtype DocumentId = DocumentId { unDocumentId :: Text }

root :: Root
root = Root "projects/stanleystots/databases/(default)/documents"

podcastCollection :: Collection
podcastCollection = Collection "podcasts"

getDocument :: Root -> Collection -> DocumentId -> IO Document
getDocument r c d = do
  lgr <- newLogger Debug stdout
  env <- newEnv <&> (envLogger .~ lgr) . (envScopes .~ datastoreScope)
  let req = unRoot r <> "/" <> unCollection c <> "/" <> unDocumentId d
  runResourceT . runGoogle env . send $ projectsDatabasesDocumentsGet req

writeDocument :: Root -> Collection -> Document -> IO Document
writeDocument r c d = do
  lgr <- newLogger Debug stdout
  env <- newEnv <&> (envLogger .~ lgr) . (envScopes .~ datastoreScope)
  runResourceT . runGoogle env . send
    $ projectsDatabasesDocumentsCreateDocument (unRoot r) (unCollection c) d

getTextValue :: HashMap Text Value -> Text -> Maybe Text
getTextValue m k = (m ^@? iix k)^?(_Just._2) >>= view vStringValue

lastSlash :: Text -> Text
lastSlash = Text.reverse . Text.takeWhile (/= '/') . Text.reverse

toPodcastDocument :: Document -> Maybe PodcastDocument
toPodcastDocument d =
  d^.dFields^?(_Just.dfAddtional) >>= fromMap (d^.dName) (d^.dCreateTime)
    where
      fromMap n t m = PodcastDocument
        <$> fmap lastSlash n
        <*> t
        <*> getTextValue m "genre"
        <*> getTextValue m "host"
        <*> getTextValue m "name"

-- getPodcast "d0HLU6SHlnKlHeuV1DAB"
-- TODO replace Text with DocumentId
getPodcast :: Text -> IO (Maybe PodcastDocument)
getPodcast =
  fmap toPodcastDocument . getDocument root podcastCollection . DocumentId

mkStringValue :: Text -> Value
mkStringValue v = value & vStringValue ?~ v

mkPodcastHashMap :: Text -> Text -> Text -> HashMap Text Value
mkPodcastHashMap n h g = mempty
  & at "name" ?~ mkStringValue n
  & at "host" ?~ mkStringValue h
  & at "genre" ?~ mkStringValue g

-- TODO use newtypes instead of Text
createPodcast :: Text -> Text -> Text -> IO (Maybe PodcastDocument)
createPodcast n h g = do
  t <- Just <$> getCurrentTime
  let d = document
            & dUpdateTime .~ t
            & dCreateTime .~ t
            & dFields ?~ documentFields (mkPodcastHashMap n h g)
  toPodcastDocument <$> writeDocument root podcastCollection d
