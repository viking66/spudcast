{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Spudcast.AudioTags
  ( readTags
  , writeTags
  ) where

import Control.Lens
import Sound.HTagLib

import Spudcast.Types

data AudioTags = AudioTags
  { _title :: Title
  , _artist :: Artist
  , _album :: Album
  , _year :: Maybe Year
  , _trackNumber :: Maybe TrackNumber
  , _genre :: Genre
  , _comment :: Comment
  , _duration :: Duration
  }
makeFieldsNoPrefix ''AudioTags

toReadTags :: AudioTags -> ReadTags
toReadTags tags = ReadTags
  { _title = unTitle (tags^.title)
  , _artist = unArtist (tags^.artist)
  , _album = unAlbum (tags^.album)
  , _year = unYear <$> (tags^.year)
  , _trackNumber = unTrackNumber <$> (tags^.trackNumber)
  , _genre = unGenre (tags^.genre)
  , _comment = unComment (tags^.comment)
  , _duration = unDuration (tags^.duration)
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
writeTags fp tags = setTags fp Nothing $
     titleSetter (mkTitle $ tags^.title)
  <> artistSetter (mkArtist $ tags^.artist)
  <> albumSetter (mkAlbum $ tags^.album)
  <> yearSetter (mkYear $ tags^.year)
  <> trackNumberSetter (mkTrackNumber $ tags^.trackNumber)
  <> genreSetter (mkGenre $ tags^.genre)
  <> commentSetter (mkComment $ tags^.comment)
