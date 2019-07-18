module Spudcast.Feed
  ( podcastItem
  ) where

import Data.Text (Text, pack)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime)
import Data.Time.Format ( defaultTimeLocale
                        , formatTime
                        )
import Data.UUID (UUID)
import qualified Data.UUID as UUID

import Spudcast.Types (ReadTags (..))

formatDuration :: Int -> Text
formatDuration n = Text.intercalate ":" $ map toText xs
  where
    xs = [ n `div` 3600
         , n `mod` 3600 `div` 60
         , n `mod` 60 ]
    toText x = pack $ (if x < 10 then "0" else "") <> show x

podcastItem :: ReadTags -> Text -> UUID -> UTCTime -> Integer -> Text
podcastItem ReadTags{..} fn uuid timestamp len = Text.intercalate "\n" xs
  where
    xs = [ "<item>"
         , "  <title>" <> title <> "</title>"
         , "  <description>"
         , "    <![CDATA[<p>" <> comment <> "</p>]]>"
         , "  </description>"
         , "  <itunes:title>" <> title <> "</itunes:title>"
         , "  <itunes:episodeType>full</itunes:episodeType>"
         , "  <itunes:episode>" <> pack (show $ maybe 0 id trackNumber) <> "</itunes:episode>"
         , "  <itunes:summary>" <> comment <> "</itunes:summary>"
         , "  <guid isPermaLink=\"false\">" <> UUID.toText uuid <> "</guid>"
         , "  <pubDate>" <> pack (formatTime defaultTimeLocale "%a, %d %b %Y %T %z" timestamp) <> "</pubDate>"
         , "  <itunes:explicit>yes</itunes:explicit>"
         , "  <itunes:image href=\"http://www.stanleystots.com/stanleys_tots.jpg\"/>"
         , "  <itunes:duration>" <> formatDuration duration <> "</itunes:duration>"
         , "  <enclosure url=\"http://www.stanleystots.com/" <> fn <> "\" type=\"audio/mpeg\" length=\"" <> pack (show len) <> "\"/>"
         , "</item>"
         ]
