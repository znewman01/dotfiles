#!/bin/sh
set -e

EMACS=/home/zjn/.nix-profile/bin/emacs

source "${BASH_SOURCE%/*}/restic_common.sh"

MAX_AGE="5 days"

function main {
    for BACKUP_SPEC in $BACKUP_SPECS; do
        LOCAL="$(echo ${BACKUP_SPEC} | $SED 's/.*://g')"
        NAME="$(echo ${BACKUP_SPEC} | $SED 's/:.*//g')"
        REMOTE="gs:${BUCKET_NAME}:/${HOSTNAME}/${NAME}"

        latest=$(run_restic -r "$REMOTE" snapshots --last --json | $JQ "sort_by(.time)[-1].time" | $SED 's/"//g')
        target=$("$DATE" -Iseconds --date "$MAX_AGE ago")
        if [[ "$latest" < "$target" ]]; then
            cat > /tmp/test.eml <<EOF
To: z@znewman.net
From: systemd <systemd@z.znewman.net>
Subject: No backups for $HOSTNAME in the last $MAX_AGE!

Last backup was on $latest! Maybe do something about it...
EOF
            $EMACS --batch \
                --load "${HOME}/.emacs.d/init.el" \
                --visit /tmp/test.eml \
                --eval "(require 'mu4e)" \
                --eval "(mu4e-context-switch nil \"Fastmail\")" \
                --eval "(smtpmail-send-it)" \
                --kill
            exit 1
        fi
    done
}

main
