#!/usr/bin/env sh

TITLE="Stanley's Tots"
DESCRIPTION="<p>I'm Stanley Randall and this is my podcast. I hope you find it entertaining.</p>"
LINK="http://www.stanleystots.com"
HOST="Stanley Randall"
EMAIL="stanleys.tots@gmail.com"
EXPLICIT=true
CATEGORY="Comedy"

curl localhost:8080/podcast -F newPodcastDetails="{\"title\": \"$TITLE\", \"description\": \"$DESCRIPTION\", \"link\": \"$LINK\", \"host\": \"$HOST\", \"email\": \"$EMAIL\", \"explicit\": $EXPLICIT, \"category\": \"$CATEGORY\"};type=application/json" -F image=@"$1"
