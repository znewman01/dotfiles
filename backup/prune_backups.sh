#!/bin/sh
set -e

source "${BASH_SOURCE%/*}/restic_common.sh"

function main {
    for BACKUP_SPEC in $BACKUP_SPECS; do
        NAME="$(echo ${BACKUP_SPEC} | sed 's/:.*//g')"
        REMOTE="gs:${BUCKET_NAME}:/${HOSTNAME}/${NAME}"

        run_restic -r "$REMOTE" forget --prune \
            --keep-hourly 24 \
            --keep-daily 7 \
            --keep-weekly 5 \
            --keep-monthly 36
    done
}

main
