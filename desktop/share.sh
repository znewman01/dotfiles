#!/usr/bin/env bash
set -eo pipefail
set -x


main() {
    if [ $# -eq 1 ]; then
        echo "Usage: $0 [FILE_TO_SHARE]"
        echo ""
        echo "Shares the file or clipboard contents via Dropbox."
        exit 1
    elif [ $# -eq 0 ]; then
        FILE="$(xclip -o -selection clipboard)"
        if [ ! -f "$FILE" ]; then
            echo "If called without arguments, clipboard must be a path to a file!"
            exit 1
        fi
    else
        FILE="$1"
    fi

    SUCCESS=0
    LINK=$(dropbox sharelink $FILE) && SUCCESS=1
    if [ $SUCCESS -eq 0 ]; then
        NEW_PATH="$HOME/Dropbox/Public/$(date '+%Y%m%d-%H%M%S')-$(basename $FILE)"
        cp $FILE $NEW_PATH
        LINK=$(dropbox sharelink $NEW_PATH) || notify-send "Sharing $FILE failed."
    fi
    echo $LINK
    echo $LINK | xclip -selection clipboard
    notify-send "Share link copied to clipboard: $LINK"
}

main $@
