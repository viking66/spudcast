module Spudcast.DB
  ( createPodcast
  , getPodcast
  ) where

import Control.Lens
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time ( getCurrentTime
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
                                , vBooleanValue
                                , vStringValue
                                , value
                                )
import System.IO (stdout)

import Spudcast.Types ( NewPodcast (..)
                      , PodcastCreateDate (..)
                      , PodcastDescription (..)
                      , PodcastDetails (..)
                      , PodcastEmail (..)
                      , PodcastExplicit (..)
                      , PodcastHost (..)
                      , PodcastId (..)
                      , PodcastImage (..)
                      , PodcastLink (..)
                      , PodcastTitle (..)
                      , mkPodcastCategory
                      , unPodcastCategory
                      )

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

lastSlash :: Text -> Text
lastSlash = Text.reverse . Text.takeWhile (/= '/') . Text.reverse

toPodcastDescription :: Document -> Maybe PodcastDetails
toPodcastDescription d =
  d^.dFields^?(_Just.dfAddtional) >>= fromMap (d^.dName) (d^.dCreateTime)
    where
      fromMap n t m = PodcastDetails
        <$> (PodcastId . lastSlash <$> n)
        <*> (PodcastCreateDate <$> t)
        <*> (PodcastTitle <$> getTextValue m "title")
        <*> (PodcastDescription <$> getTextValue m "description")
        <*> (PodcastLink <$> getTextValue m "link")
        <*> (PodcastHost <$> getTextValue m "host")
        <*> (PodcastEmail <$> getTextValue m "email")
        <*> (PodcastExplicit <$> getBoolValue m "explicit")
        <*> (mkPodcastCategory <$> getTextValue m "category")
        <*> (PodcastImage <$> getTextValue m "imageTitle" <*> getTextValue m "imageUrl")

-- getPodcast "d0HLU6SHlnKlHeuV1DAB"
getPodcast :: PodcastId -> IO (Maybe PodcastDetails)
getPodcast = fmap toPodcastDescription
  . getDocument root podcastCollection
  . DocumentId
  . unPodcastId

mkStringValue :: Text -> Value
mkStringValue v = value & vStringValue ?~ v

mkBooleanValue :: Bool -> Value
mkBooleanValue v = value & vBooleanValue ?~ v

mkPodcastHashMap :: NewPodcast -> HashMap Text Value
mkPodcastHashMap NewPodcast{..} = mempty
  & at "title" ?~ mkStringValue (unPodcastTitle title)
  & at "description" ?~ mkStringValue (unPodcastDescription description)
  & at "link" ?~ mkStringValue (unPodcastLink link)
  & at "host" ?~ mkStringValue (unPodcastHost host)
  & at "email" ?~ mkStringValue (unPodcastEmail email)
  & at "explicit" ?~ mkBooleanValue (unPodcastExplicit explicit)
  & at "category" ?~ mkStringValue (unPodcastCategory category)
  & at "imageTitle" ?~ mkStringValue (imageTitle image)
  & at "imageUrl" ?~ mkStringValue (imageUrl image)

createPodcast :: NewPodcast -> IO (Maybe PodcastDetails)
createPodcast x = do
  t <- Just <$> getCurrentTime
  let d = document
            & dUpdateTime .~ t
            & dCreateTime .~ t
            & dFields ?~ documentFields (mkPodcastHashMap x)
  toPodcastDescription <$> writeDocument root podcastCollection d
