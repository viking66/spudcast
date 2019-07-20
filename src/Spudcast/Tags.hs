module Spudcast.Tags
  ( readTags
  , writeTags
  ) where

import Sound.HTagLib

import Spudcast.Types ( ReadTags (..)
                      , WriteTags (..)
                      )

data AudioTags = AudioTags
  { title :: Title
  , artist :: Artist
  , album :: Album
  , year :: Maybe Year
  , trackNumber :: Maybe TrackNumber
  , genre :: Genre
  , comment :: Comment
  , duration :: Duration
  }

toReadTags :: AudioTags -> ReadTags
toReadTags AudioTags{..} = ReadTags
  { title = unTitle title
  , artist = unArtist artist
  , album = unAlbum album
  , year = unYear <$> year
  , trackNumber = unTrackNumber <$> trackNumber
  , genre = unGenre genre
  , comment = unComment comment
  , duration = unDuration duration
  }

audioTagGetter :: TagGetter AudioTags
audioTagGetter = AudioTags
  <$> titleGetter
  <*> artistGetter
  <*> albumGetter
  <*> yearGetter
  <*> trackNumberGetter
  <*> genreGetter
  <*> commentGetter
  <*> durationGetter

readTags :: FilePath -> IO ReadTags
readTags fp = toReadTags <$> getTags fp audioTagGetter

writeTags :: FilePath -> WriteTags -> IO ()
writeTags fp WriteTags{..} = setTags fp Nothing $
     titleSetter (mkTitle title)
  <> artistSetter (mkArtist artist)
  <> albumSetter (mkAlbum album)
  <> yearSetter (mkYear year)
  <> trackNumberSetter (mkTrackNumber trackNumber)
  <> genreSetter (mkGenre genre)
  <> commentSetter (mkComment comment)
