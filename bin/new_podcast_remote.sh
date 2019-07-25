#! /usr/bin/env nix-shell
#! nix-shell -i bash -p google-cloud-sdk

PODCAST="Stanley's Tots"
HOST="Stanley Randall"
GENRE="Comedy"

TITLE="FOO"
EPISODE=666
DESCRIPTION="BAR"

curl -H "Authorization: Bearer $(gcloud config config-helper --format 'value(credential.id_token)')" https://spudcast-c4zbgpzdpa-uc.a.run.app/podcast -F episodeDetails="{\"podcastName\": \"$PODCAST\", \"host\": \"$HOST\", \"genre\": \"$GENRE\", \"epTitle\": \"$TITLE\", \"epNumber\": $EPISODE, \"epDescription\": \"$DESCRIPTION\"};type=application/json" -F audio=@"$1"
