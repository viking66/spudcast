module Spudcast.Handlers
  ( uploadPodcast
  ) where

import Servant.Server (Handler)

-- import Spudcast.Storage (write)
import Spudcast.API ( EpisodeDetails
                    , PodcastEpisode
                    , getEpisodeDetails
                    )

uploadPodcast :: PodcastEpisode -> Handler EpisodeDetails
uploadPodcast = pure . getEpisodeDetails
