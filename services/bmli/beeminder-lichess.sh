#!/bin/sh
set -eux
BEE="https://www.beeminder.com/api/v1"
BEE_ME="${BEE}/users/znewman01/goals/lichess-fast"
test -z "$BEE_AUTH" && exit 1 || true
BEE_AUTH="auth_token=${BEE_AUTH}"

curl -s "${BEE_ME}/datapoints.json?${BEE_AUTH}&count=1" > /tmp/datapoint.json
BEE_GAMES=$(jq first.value /tmp/datapoint.json)
LI_GAMES=$(curl -s https://lichess.org/api/user/znewman01 | jq '.perfs.bullet.games + .perfs.blitz.games + .perfs.rapid.games')

test $LI_GAMES -eq $BEE_GAMES && exit 0 || true  # Beeminder up-to-date

DATAPOINT_ID=$(curl -s -X POST \ --data "${BEE_AUTH}&value=${LI_GAMES}" "${BEE_ME}/datapoints.json" | jq -r '.id')

sleep 1  # give time to update

BAREMIN=$(curl -s "${BEE_ME}?${BEE_AUTH}&count=1" | jq '.baremin' | tr -d '"+')
test $BAREMIN -ge 0 && exit 0 || true  # didn't derail

TOTAL_CHARGE=$(expr 0 - $BAREMIN || true)
ALREADY_CHARGED=$(jq -r 'try (first.comment | tonumber) catch 0' /tmp/datapoint.json)
TO_CHARGE=$(expr $TOTAL_CHARGE - $ALREADY_CHARGED || true)

test $TO_CHARGE -gt 0 && echo curl -s -X POST "${BEE}/charges.json?${BEE_AUTH}&amount=${TO_CHARGE}&note=lichess-fast"  || true  # charging
curl -X PUT "${BEE_ME}/datapoints/${DATAPOINT_ID}.json?${BEE_AUTH}&comment=$TOTAL_CHARGE"
