#!/bin/bash

URL="$1"
echo "# waiting (up to ${TIMEOUT:=5} sec) for $URL" >&2

START="$(date +%s)";
while (! (curl -sf "$URL")) \
          && (($(date +%s) - START < TIMEOUT))
do sleep 0.05; done

curl -sf "$URL" > /dev/null || false
