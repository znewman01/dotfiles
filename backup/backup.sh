#!/bin/sh
set -e

source ./restic_common.sh

function main {
    for BACKUP_SPEC in $BACKUP_SPECS; do
        LOCAL="$(echo ${BACKUP_SPEC} | sed 's/.*://g')"
        NAME="$(echo ${BACKUP_SPEC} | sed 's/:.*//g')"
        REMOTE="gs:${BUCKET_NAME}:/${HOSTNAME}/${NAME}"

        cd "$LOCAL"
        run_restic -r "$REMOTE" backup .
    done
}

main
