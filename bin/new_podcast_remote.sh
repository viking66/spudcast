#!/usr/bin/env sh

PODCAST="Stanley's Tots"
HOST="Stanley Randall"
GENRE="Comedy"

TITLE="Potato test"
EPISODE=666
DESCRIPTION="Trying some shit out"

curl -H "Authorization: Bearer $(gcloud config config-helper --format 'value(credential.id_token)')" https://spudcast-c4zbgpzdpa-uc.a.run.app/podcast -F episodeDetails="{\"podcastName\": \"$PODCAST\", \"host\": \"$HOST\", \"genre\": \"$GENRE\", \"epTitle\": \"$TITLE\", \"epNumber\": $EPISODE, \"epDescription\": \"$DESCRIPTION\"};type=application/json" -F audio=@"$1"
