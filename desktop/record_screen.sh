#! /usr/bin/env nix-shell
#! nix-shell -i bash -p screenkey byzanz slop xdotool xorg.xrandr libnotify bash procps utillinux coreutils gnugrep xclip xcompmgr
# Record the given X screen into an animated GIF. Run again to finish the recording.
set -eo pipefail
set -x

# This should be toggle-able. That is, running this script once should start a
# recording; running it again should terminate the prior recording.
#
# To do this, we use the `--exec` flag of Byzanz: create a Bash process that
# records its own PID to a file in a well-known location then hangs. If this
# file exists, we kill it and exit this script.

# The well-known location:
PID_FILE="${TMPDIR}/byzanz.pid"
RECORDING_PATH_FILE="${TMPDIR}/byzanz-path"
LOCK_FILE="${TMPDIR}/byzanz.lock"

# Directory to save recordings in
RECORDINGS_DIR="$HOME/Sync/recordings"
# Recording will be called "${RECORDING_PREFIX}<date>"
RECORDING_PREFIX="recording-"

cleanup() {
    pkill screenkey || true
    rm -f "${PID_FILE}" || true
    rm -f "${LOCK_FILE}" || true
    rm -f "${RECORDING_PATH_FILE}" || true
}
# If a recording is already happening, terminate it.
exit_and_stop_recording() {
    kill $(cat "${PID_FILE}")
    pkill screenkey || true

    RECORDING_PATH=$(cat "${RECORDING_PATH_FILE}")
    while [ ! -f "$RECORDING_PATH" ]; do
        sleep 0.1
    done
    echo $RECORDING_PATH | xclip -selection clipboard
    notify-send "Recording ready (and path copied to clipboard)"
    cleanup

    exit 0
}

# This is shockingly complicated; X considers my desktop to be one big screen so
# I have to compute this.
get_screen_for_point () {
    local SCREENS WIDTH HEIGH X_OFFSET Y_OFFSET
    local X=$1 Y=$2

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

# Otherwise, start a recording of the current 
start_recording() {
    local SCREEN X Y WIDTH HEIGHT
    # Sets above variables
    eval "$(xdotool getactivewindow getwindowgeometry --shell)"
    unset SCREEN  # xdotool thinks everything's on the same screen

    if [ "$1" == "window" ]; then
        # Active window
        true  # X, Y, WIDTH, HEIGHT already set from xdotool
    elif [ "$1" == "fullscreen" ]; then
        # Fullscreen; current screen
        read WIDTH HEIGHT X Y <<< $( \
            get_screen_for_point $X $Y \
                | tr 'x,' '  ' \
        )
    elif [ "$1" == "all" ]; then
        read WIDTH HEIGHT <<< $( \
            xrandr \
                | grep -Eo 'current [0-9]+ x [0-9]+' \
                | cut -d' ' -f 2,4 \
        )
        X=0
        Y=0
    else
        # Select rectangle, or if you click on a window just that window
        FAILED=0
        notify-send --expire-time 200 "Select window/rect for recording"
        SLOP="$(slop)" || FAILED=1
        if [ $FAILED -eq 1 ]; then
            notify-send "Recording aborted"
            cleanup
            exit 1
        fi
        read WIDTH HEIGHT X Y <<< $(echo "$SLOP" | tr 'x+' '  ') 
        if [ $WIDTH -eq 0 ] || [ $HEIGHT -eq 0 ]; then
            eval "$(xdotool getmouselocation --shell | grep WINDOW)"
            eval "$(xdotool getwindowgeometry --shell $WINDOW)"
        fi 
    fi

    RECORDING_PATH="$RECORDINGS_DIR/${RECORDING_PREFIX}$(date '+%Y%m%d-%H%M%S').gif"
    echo $RECORDING_PATH > $RECORDING_PATH_FILE
    mkdir -p "$RECORDINGS_DIR"
    DELAY=2  # screenkey takes a sec to get started
    byzanz-record \
        --cursor \
        --delay $DELAY \
        --x=$X --y=$Y \
        --width=$WIDTH --height=$HEIGHT \
        --exec "bash -c 'echo \$BASHPID > $PID_FILE && sleep infinity'" \
        "$RECORDING_PATH" &

    sleep 0.2
    SKEY_WIDTH=$WIDTH
    SKEY_HEIGHT=$(expr $HEIGHT / 2)
    SKEY_X=$X
    SKEY_Y=$(expr $Y + $HEIGHT - $SKEY_HEIGHT)
    screenkey \
        --persist \
        --geometry "${SKEY_WIDTH}x${SKEY_HEIGHT}+${SKEY_X}+${SKEY_Y}" &
}

main() {
    if [ -f "${PID_FILE}" ]; then
        # repeated executions toggle recording
        exit_and_stop_recording  
    fi
    if [ -f "${LOCK_FILE}" ]; then
        notify-send --expire-time 1000 "Another recording process is running; please resolve ${LOCK_FILE}."
        exit 1
    fi
    touch ${LOCK_FILE}  # there's a TOCTOU issue here but I'm not too bothered
    start_recording $@
}

main $@
