#!/bin/sh
set -e

source "${BASH_SOURCE%/*}/restic_common.sh"

function main {
    for BACKUP_SPEC in $BACKUP_SPECS; do
        LOCAL="$(echo ${BACKUP_SPEC} | $SED 's/.*://g')"
        NAME="$(echo ${BACKUP_SPEC} | $SED 's/:.*//g')"
        REMOTE="gs:${BUCKET_NAME}:/${HOSTNAME}/${NAME}"

        cd "$LOCAL"
        run_restic -r "$REMOTE" backup .
    done
}

main
