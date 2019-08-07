#!/usr/bin/env sh

PODCAST="Stanley's Tots"
HOST="Stanley Randall"
GENRE="Comedy"

TITLE="FOO"
EPISODE=666
DESCRIPTION="BAR"

curl localhost:8080/podcast/tots -F episodeDetails="{\"podcastName\": \"$PODCAST\", \"host\": \"$HOST\", \"genre\": \"$GENRE\", \"epTitle\": \"$TITLE\", \"epNumber\": $EPISODE, \"epDescription\": \"$DESCRIPTION\"};type=application/json" -F audio=@"$1"
