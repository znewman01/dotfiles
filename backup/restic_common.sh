RESTIC={{RESTIC_BIN_PATH}}
PASS={{PASS_BIN_PATH}}
JQ={{JQ_BIN_PATH}}
SED={{SED_BIN_PATH}}
CAT={{SED_BIN_PATH}}
DATE={{DATE_BIN_PATH}}

BUCKET_NAME=zjn-backup
BACKUP_SPECS="
passwords:$HOME/Dropbox/passwords
keys:$HOME/Dropbox/keys
org:$HOME/Dropbox/notes
"

export GOOGLE_PROJECT_ID=999013580212
export GOOGLE_APPLICATION_CREDENTIALS=$HOME/Dropbox/keys/gcp-service-account.json

function run_restic {
    "$RESTIC" \
        --password-command="${PASS} backups" \
        --verbose \
        $@
}
