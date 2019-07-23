#!/usr/bin/env sh

PODCAST="Stanley's Tots"
HOST="Stanley Randall"
GENRE="Comedy"

TITLE="Potato test"
EPISODE=666
DESCRIPTION="Trying some shit out"

curl localhost:8080/podcast -F episodeDetails="{\"podcastName\": \"$PODCAST\", \"host\": \"$HOST\", \"genre\": \"$GENRE\", \"epTitle\": \"$TITLE\", \"epNumber\": $EPISODE, \"epDescription\": \"$DESCRIPTION\"};type=application/json" -F audio=@"$1"
