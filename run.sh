#!/usr/bin/env sh

PODCAST="Stanley's Tots"
HOST="Stanley Randall"
GENRE="Comedy"

TITLE="Cheddar & Sour Cream Ruffles"
EPISODE=6
DESCRIPTION="Stanley goes on a read trip and finds the pog that got away."
INPUT="./stanleys_tots_20190715.mp3"

ITEM=$(cabal new-run spudcast -- \
  --podcast-name "$PODCAST" \
  --host "$HOST" \
  --genre "$GENRE" \
  --ep-title "$TITLE" \
  --ep-number $EPISODE \
  --ep-description "$DESCRIPTION" \
  --input "$INPUT")

OUTPUT=$(grep file <<< "$ITEM" | sed 's/file=//g')

gsutil cp "$OUTPUT" gs://www.stanleystots.com/
echo "$ITEM"
