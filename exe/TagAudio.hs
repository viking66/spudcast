module Main where

import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock ( UTCTime (..)
                       , getCurrentTime
                       )
import Data.Time.Format ( defaultTimeLocale
                        , formatTime
                        )
import Data.UUID ( UUID
                 , toText
                 )
import Data.UUID.V4 (nextRandom)
import Options.Applicative
import System.Directory ( copyFile
                        , getFileSize
                        )
import System.FilePath (replaceFileName)

import Spudcast.Feed (podcastItem)
import Spudcast.Tags ( readTags
                     , writeTags
                     )
import Spudcast.Types (WriteTags (..))

data AppArgs = AppArgs
  { podcastName :: Text
  , host :: Text
  , genre :: Text
  , episodeTitle :: Text
  , episodeNumber :: Int
  , episodeDescription :: Text
  , input :: Text
  }
  deriving Show

appArgs :: Parser AppArgs
appArgs = AppArgs
  <$> strOption
     ( long "podcast-name"
    <> metavar "NAME"
    <> help "Name of podcast" )
  <*> strOption
     ( long "host"
    <> metavar "HOST"
    <> help "Name of podcast host" )
  <*> strOption
     ( long "genre"
    <> metavar "GENRE"
    <> help "Genre of the podcast" )
  <*> strOption
     ( long "ep-title"
    <> metavar "TITLE"
    <> help "Title of podcast episode" )
  <*> option auto
     ( long "ep-number"
    <> metavar "INT"
    <> help "Episode number" )
  <*> strOption
     ( long "ep-description"
    <> metavar "DESCRIPTION"
    <> help "Description of the podcast episode" )
  <*> strOption
     ( long "input"
    <> short 'i'
    <> metavar "FILE"
    <> help "Audio file to process" )

getArgs :: IO AppArgs
getArgs = execParser opts
  where
    opts = info (appArgs <**> helper)
       ( fullDesc
      <> progDesc "Prepare audio file for podcast upload"
      <> header "spudcast - manage podcast content" )

toWriteTags :: AppArgs -> UTCTime -> WriteTags
toWriteTags AppArgs{..} t = WriteTags
  { title = episodeTitle
  , artist = host
  , album = podcastName
  , year = getYear t
  , trackNumber = episodeNumber
  , genre = genre
  , comment = episodeDescription
  }

getYear :: UTCTime -> Int
getYear = fromInteger . getYear' . toGregorian . utctDay
  where getYear' (y, _, _) = y

outfile :: UUID -> UTCTime -> Text
outfile u t = Text.intercalate "." [toText u, timePart, "mp3"]
  where timePart = Text.pack $ formatTime defaultTimeLocale "%Y%m%d%H%M" t

main :: IO ()
main = do
  args <- getArgs
  timestamp <- getCurrentTime
  uuid <- nextRandom
  let fn = outfile uuid timestamp
      out = replaceFileName (Text.unpack $ input args) (Text.unpack $ fn)
  _ <- copyFile (Text.unpack $ input args) out
  _ <- writeTags out $ toWriteTags args timestamp
  tags <- readTags out
  filesize <- getFileSize out
  _ <- putStrLn $ "file=" <> out
  putStrLn . Text.unpack $ podcastItem tags fn uuid timestamp filesize
