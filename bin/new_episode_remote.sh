#! /usr/bin/env nix-shell
#! nix-shell -i bash -p google-cloud-sdk

PODCAST="DT68WjPHZReJ8BUfYEHl"

TITLE="FOO"
NUMBER=666
DESCRIPTION="BAR"

curl -H "Authorization: Bearer $(gcloud config config-helper --format 'value(credential.id_token)')" https://spudcast-c4zbgpzdpa-uc.a.run.app/podcast/$PODCAST -F episodeDetails="{\"title\": \"$TITLE\", \"number\": $NUMBER, \"description\": \"$DESCRIPTION\"};type=application/json" -F audio=@"$1"
