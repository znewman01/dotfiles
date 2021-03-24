#! /usr/bin/env nix-shell
#! nix-shell -i bash -p maim xclip libnotify imagemagick xdotool
set -x
set -eo pipefail

SCREENSHOT_DIR="$HOME/Sync/screenshots"
SCREENSHOT_PREFIX="screenshot-"

# Copied from record_screen.sh
get_current_screen_geometry () {
    local SCREENS WIDTH HEIGH X_OFFSET Y_OFFSET

    eval "$(xdotool getactivewindow getwindowgeometry --shell)"

    NUM="[0-9]+"
    # ex 2560x1440+0+0
    SCREENS=$(xrandr | grep -Eo "${NUM}x${NUM}\+${NUM}\+${NUM}")
    for screen in $SCREENS; do
        read WIDTH HEIGHT X_OFFSET Y_OFFSET <<< $( \
            echo $screen \
                | tr 'x+' '  ' \
        )
        if [ $X -gt $X_OFFSET ] && \
           [ $X -lt $(expr $X_OFFSET + $WIDTH) ] && \
           [ $Y -gt $Y_OFFSET ] && \
           [ $X -lt $(expr $X_OFFSET + $WIDTH) ]; then
            # coords are within this screen
            echo $screen | tr 'x+' '  '
            exit 0
        fi
    done
    exit 1  # no match found
}

main () {
    mkdir -p "$SCREENSHOT_DIR"
    SCREENSHOT_PATH="$SCREENSHOT_DIR/$SCREENSHOT_PREFIX$(date '+%Y%m%d-%H%M%S').png"
    if [ "$1" == "fullscreen" ]; then
        read WIDTH HEIGHT X Y <<< $(get_current_screen_geometry)
        maim --hidecursor --geometry="${WIDTH}x${HEIGHT}+${X}+${Y}" > "$SCREENSHOT_PATH"
    elif [[ "$1" =~ ^#[0-9a-fA-F]{6}$ ]]; then
        maim --hidecursor | convert - -background "$1" -alpha remove -alpha off - > "$SCREENSHOT_PATH"
    else
        maim --hidecursor $@ > "$SCREENSHOT_PATH"
    fi

    echo $SCREENSHOT_PATH | xclip -selection clipboard
    notify-send "Screenshot captured (and in your clipboard): $SCREENSHOT_PATH"
}

main $@
