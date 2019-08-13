#!/usr/bin/env sh

PODCAST="DT68WjPHZReJ8BUfYEHl"

TITLE="FOO"
NUMBER=666
DESCRIPTION="BAR"

curl localhost:8080/podcast/$PODCAST -F episodeDetails="{\"title\": \"$TITLE\", \"number\": $NUMBER, \"description\": \"$DESCRIPTION\"};type=application/json" -F audio=@"$1"
