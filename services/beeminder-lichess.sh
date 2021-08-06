#! nix-shell -i bash -p jq curl pass
set -euo pipefail
GOAL=lichess-fast
BEE="https://www.beeminder.com/api/v1"
BEE_AUTH="auth_token=$(pass show beeminder-auth-token)"

TMP_FILE=$(mktemp)
curl -s "${BEE}/users/znewman01/goals/lichess-fast/datapoints.json?${BEE_AUTH}&count=1" > $TMP_FILE
BEE_GAMES=$(jq first.value $TMP_FILE)
echo "Current Beeminder # of games: ${BEE_GAMES}"

LI_GAMES=$(curl -s https://lichess.org/api/user/znewman01 | jq '.perfs.bullet.games + .perfs.blitz.games')
echo "Current Lichess # of games: ${LI_GAMES}"

if [ $LI_GAMES -eq $BEE_GAMES ]; then
    echo "Beeminder is up-to-date; exiting."
    exit 0
fi

echo "Posting the new data point..."
DATAPOINT_ID=$(curl -s -X POST \
    --data "${BEE_AUTH}&value=${LI_GAMES}" \
    "${BEE}/users/znewman01/goals/${GOAL}/datapoints.json" \
    | jq '.id' | sed 's/"//g')

sleep 1

BAREMIN=$(curl -s "${BEE}/users/znewman01/goals/lichess-fast?${BEE_AUTH}&count=1" | jq '.baremin' | sed 's/["+]//g')
if [ $BAREMIN -ge 0 ]; then
    echo "Didn't derail!"
    exit
fi

TOTAL_CHARGE=$(expr 0 - $BAREMIN || true)
echo "Total to charge: $TOTAL_CHARGE"

ALREADY_CHARGED=$(jq 'try (first.comment | tonumber) catch 0' $TMP_FILE | sed 's/"//g')
echo "Already charged today: ${ALREADY_CHARGED}"

TO_CHARGE=$(expr $TOTAL_CHARGE - $ALREADY_CHARGED || true)
echo "Need to charge: ${TO_CHARGE}"

if [ $TO_CHARGE -gt 0 ]; then
    echo "Charging..."
    echo "(not really)"
    echo curl -s -X POST "${BEE}/charges.json?${BEE_AUTH}&amount=${TO_CHARGE}&note=lichess-fast"
else
    echo "Not charging."
fi
echo curl -X PUT "${BEE}/users/znewman01/goals/lichess-fast/datapoints/${DATAPOINT_ID}.json?${BEE_AUTH}&comment=$TOTAL_CHARGE"
curl -X PUT "${BEE}/users/znewman01/goals/lichess-fast/datapoints/${DATAPOINT_ID}.json?${BEE_AUTH}&comment=$TOTAL_CHARGE"

rm -f "${TMP_FILE}"
