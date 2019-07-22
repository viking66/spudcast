module Spudcast.Handlers
  ( uploadPodcast
  ) where

-- import Spudcast.Storage (write)
import Spudcast.API ( EpisodeDetails
                    , PodcastEpisode
                    , getEpisodeDetails
                    )

uploadPodcast :: PodcastEpisode -> IO EpisodeDetails
uploadPodcast = pure . getEpisodeDetails
