#! /usr/bin/env nix-shell
#! nix-shell -i bash -p google-cloud-sdk

TITLE="Stanley's Tots"
DESCRIPTION="<p>I'm Stanley Randall and this is my podcast. I hope you find it entertaining.</p>"
LINK="http://www.stanleystots.com"
HOST="Stanley Randall"
EMAIL="stanleys.tots@gmail.com"
EXPLICIT=true
CATEGORY="Comedy"

curl -H "Authorization: Bearer $(gcloud config config-helper --format 'value(credential.id_token)')" https://spudcast-c4zbgpzdpa-uc.a.run.app/podcast -F newPodcastDetails="{\"title\": \"$TITLE\", \"description\": \"$DESCRIPTION\", \"link\": \"$LINK\", \"host\": \"$HOST\", \"email\": \"$EMAIL\", \"explicit\": $EXPLICIT, \"category\": \"$CATEGORY\"};type=application/json" -F image=@"$1"
