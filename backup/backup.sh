#!/bin/sh
set -e

function show_help {
    echo "Usage: $0 [--dry-run] (--backup|--restore)"
}

source ./restic_common.sh

function main {
    for BACKUP_SPEC in $BACKUP_SPECS; do
        LOCAL="$(echo ${BACKUP_SPEC} | sed 's/.*://g')"
        NAME="$(echo ${BACKUP_SPEC} | sed 's/:.*//g')"
        REMOTE="gs:${BUCKET_NAME}:/${HOSTNAME}/${NAME}"

        run_restic -r "$REMOTE" backup "$LOCAL"
    done
}

main
