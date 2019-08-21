#!/bin/sh
set -e

function show_help {
    echo "Usage: $0 BACKUP_NAME [...]"
    echo
    echo "E.g. \"$0 passwords restore latest --target ...\"."
    echo "Passes through remaining arguments to restic."
}

source ./restic_common.sh

function main {
    BACKUP_NAME="$1"
    if [ -z "$BACKUP_NAME" ]; then
        show_help
        exit 1
    fi
    shift

    RAN=0
    for BACKUP_SPEC in $BACKUP_SPECS; do
        LOCAL="$(echo ${BACKUP_SPEC} | sed 's/.*://g')"
        NAME="$(echo ${BACKUP_SPEC} | sed 's/:.*//g')"
        REMOTE="gs:${BUCKET_NAME}:/${HOSTNAME}/${NAME}"

        if [ "$NAME" = "$BACKUP_NAME" ]; then
            run_restic -r "$REMOTE" $@
            RAN=1
        fi
    done

    if [ $RAN -eq 0 ]; then
        echo "Backup $BACKUP_NAME not found."
        echo
        echo "Available backups:"
        echo "$BACKUP_SPECS"
        exit 1
    fi
}

main $@
