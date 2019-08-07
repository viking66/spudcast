module Spudcast.DB
  ( createPodcast
  , getPodcast
  ) where

import Control.Lens
import Data.Text (Text)
import qualified Data.Text as Text
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
                                , vBooleanValue
                                , vStringValue
                                , value
                                )
import System.IO (stdout)

import Spudcast.Types

newtype Root = Root { unRoot :: Text }
newtype Collection = Collection { unCollection :: Text }
newtype DocumentId = DocumentId { unDocumentId :: Text }

root :: Root
root = Root "projects/stanleystots/databases/(default)/documents"

podcastCollection :: Collection
podcastCollection = Collection "podcasts"

getResourceName :: Root -> Collection -> DocumentId -> Text
getResourceName r c d =
  unRoot r <> "/" <> unCollection c <> "/" <> unDocumentId d

getDocument :: Root -> Collection -> DocumentId -> IO Document
getDocument r c d = do
  lgr <- newLogger Debug stdout
  env <- newEnv <&> (envLogger .~ lgr) . (envScopes .~ datastoreScope)
  runResourceT . runGoogle env . send $
    projectsDatabasesDocumentsGet (getResourceName r c d)

createDocument :: Root -> Collection -> Document -> IO Document
createDocument r c d = do
  lgr <- newLogger Debug stdout
  env <- newEnv <&> (envLogger .~ lgr) . (envScopes .~ datastoreScope)
  runResourceT . runGoogle env . send $
    projectsDatabasesDocumentsCreateDocument (unRoot r) (unCollection c) d

getValue :: (Ixed b)
         => Getting (Maybe a) (IxValue b) (Maybe a)
         -> b
         -> Index b
         -> Maybe a
getValue l m k = (m ^@? iix k)^?(_Just._2) >>= view l

getTextValue :: HashMap Text Value -> Text -> Maybe Text
getTextValue = getValue vStringValue

getBoolValue :: HashMap Text Value -> Text -> Maybe Bool
getBoolValue = getValue vBooleanValue

getDocumentId :: Document -> Maybe Text
getDocumentId = fmap lastSlash . view dName
  where
    lastSlash = Text.reverse . Text.takeWhile (/= '/') . Text.reverse

toPodcastDescription :: Document -> Maybe Podcast
toPodcastDescription d =
  d^.dFields^?(_Just.dfAddtional) >>= mkPodcast (getDocumentId d) (d^.dCreateTime)
    where
      mkPodcast pId t m = Podcast <$> pId <*> mkPodcastDetails t m
      mkPodcastDetails t m = PodcastDetails
        <$> t
        <*> getTextValue m "title"
        <*> getTextValue m "description"
        <*> getTextValue m "link"
        <*> getTextValue m "host"
        <*> getTextValue m "email"
        <*> getBoolValue m "explicit"
        <*> getTextValue m "category"

-- getPodcast "d0HLU6SHlnKlHeuV1DAB"
getPodcast :: PodcastId -> IO (Maybe Podcast)
getPodcast = fmap toPodcastDescription
  . getDocument root podcastCollection
  . DocumentId

mkStringValue :: Text -> Value
mkStringValue v = value & vStringValue ?~ v

mkBooleanValue :: Bool -> Value
mkBooleanValue v = value & vBooleanValue ?~ v

mkPodcastHashMap :: PodcastDetails -> HashMap Text Value
mkPodcastHashMap PodcastDetails{..} = mempty
  & at "title" ?~ mkStringValue title
  & at "description" ?~ mkStringValue description
  & at "link" ?~ mkStringValue link
  & at "host" ?~ mkStringValue host
  & at "email" ?~ mkStringValue email
  & at "explicit" ?~ mkBooleanValue explicit
  & at "category" ?~ mkStringValue category

createPodcast :: PodcastDetails -> IO (Maybe Podcast)
createPodcast p@PodcastDetails{..} = do
  let d = document
            & dUpdateTime ?~ createDate
            & dCreateTime ?~ createDate
            & dFields ?~ documentFields (mkPodcastHashMap p)
  toPodcastDescription <$> createDocument root podcastCollection d
