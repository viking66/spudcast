module Spudcast.Feed
  ( podcastItem
  ) where

import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime)
import Data.Time.Format ( defaultTimeLocale
                        , formatTime
                        )
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Text.XML.Light ( Attr (..)
                      , CData (..)
                      , CDataKind (..)
                      , Content (..)
                      , Element (..)
                      , QName (..)
                      , ppElement
                      )

import Spudcast.Types (ReadTags (..))

formatDuration :: Int -> Text.Text
formatDuration n = Text.intercalate ":" $ map toText xs
  where
    xs = [ n `div` 3600
         , n `mod` 3600 `div` 60
         , n `mod` 60 ]
    toText x = Text.pack $ (if x < 10 then "0" else "") <> show x

mkName :: Text.Text -> QName
mkName n = QName (Text.unpack n) Nothing Nothing

mkAttr :: Text.Text -> Text.Text -> Attr
mkAttr k = Attr (mkName k) . Text.unpack

mkElemAttr :: Text.Text -> [Attr] -> [Content] -> Element
mkElemAttr n as cs = Element
  { elName = mkName n
  , elAttribs = as
  , elContent = cs
  , elLine = Nothing
  }

mkElem :: Text.Text -> [Content] -> Element
mkElem n = mkElemAttr n []

textContent :: Text.Text -> Content
textContent t = Text $ CData CDataText (Text.unpack t) Nothing

cdataContent :: Text.Text -> Content
cdataContent t = Text $ CData CDataVerbatim (Text.unpack t) Nothing

mkLiteralElem :: Text.Text -> Text.Text -> Element
mkLiteralElem n c = mkElem n [textContent c]

mkLiteralelemAttr :: Text.Text -> [(Text.Text, Text.Text)] -> Text.Text -> Element
mkLiteralelemAttr n as c = mkElemAttr n as' [textContent c]
  where as' = map (uncurry mkAttr) as

mkEmptyLiteralelemAttr :: Text.Text -> [(Text.Text, Text.Text)] -> Element
mkEmptyLiteralelemAttr n as = mkElemAttr n as' []
  where as' = map (uncurry mkAttr) as

podcastItem :: ReadTags -> Text.Text -> UUID -> UTCTime -> Integer -> Text.Text
podcastItem ReadTags{..} fn uuid ts len = Text.pack . ppElement $ itemElem
  where
    titleElem = mkLiteralElem "title" title
    desc = "<p>" <> comment <> "</p>"
    descElem = mkElem "description" [cdataContent desc]
    iTitleElem = mkLiteralElem "itunes:title" title
    iTypeElem = mkLiteralElem "itunes:episodeType" "full"
    ep = Text.pack . show . fromMaybe 0 $ trackNumber
    iEpElem = mkLiteralElem "itunes:episode" ep
    iSummaryElem = mkLiteralElem "itunes:summary" comment
    guidElem = mkLiteralelemAttr "guid" [("isPermaLink", "false")] (UUID.toText uuid)
    pubDate = Text.pack . formatTime defaultTimeLocale "%a, %d %b %Y %T %z" $ ts
    pubDateElem = mkLiteralElem "pubDate" pubDate
    iExplicitElem = mkLiteralElem "itunes:explicit" "yes"
    iImageElem = mkEmptyLiteralelemAttr "itunes:image" [("href", "http://www.stanleystots.com/stanleys_tots.jpg")]
    iDurationElem = mkLiteralElem "itunes:duration" (formatDuration duration)
    url = "http://www.stanleystots.com/" <> fn
    length = Text.pack . show $ len
    enclosureElem = mkEmptyLiteralelemAttr "enclosure" [("url", url), ("type", "audio/mpeg"), ("length", length)]
    itemElem = mkElem "item" $ map Elem
      [ titleElem, descElem, iTitleElem, iTypeElem, iEpElem, iSummaryElem
      , guidElem, pubDateElem, iExplicitElem, iImageElem, iDurationElem
      , enclosureElem
      ]
