#!/bin/sh
set -e
set -x

HOST=zjn-workstation.local
MODENAME="$(hostname)-foo"

# Compute local things
RESOLUTION=$(xrandr | grep \* | awk '{print $1}')
WIDTH=$(echo $RESOLUTION | sed 's/x.*//')
HEIGHT=$(echo $RESOLUTION | sed 's/.*x//')
FREQ=$(xrandr  | grep \* | awk '{print $2}' | sed 's/\*.*//')

# Compute remote things
# Rightmost screen
PRIMARY=$(
  ssh $HOST -- DISPLAY=:0 xrandr \
    | grep ' connected' \
    | awk '{print $3,$1}' \
    | sed -E 's/[0-9]+x[0-9]+\+([0-9]+)\+[0-9]+/\1/g' \
    | sort -n \
    | tail -n 1 \
    | awk '{print $2}'
)
MODELINE=$(
  ssh $HOST -- gtf $WIDTH $HEIGHT $FREQ \
    | grep Modeline  \
    | sed 's/  */ /g'  \
    | cut -d' ' -f 4-
)
# Unused output to use for the virtual screen
OUTPUT=$(
  ssh $HOST -- DISPLAY=:0 xrandr  \
    | grep "disconnected" \
    | grep HDMI \
    | head -n 1 \
    | awk '{print $1}'
)

# Ensure we cleaned up properly last time
ssh $HOST -- DISPLAY=:0 xrandr --output $OUTPUT --off
ssh $HOST -- DISPLAY=:0 xrandr --delmode $OUTPUT $MODENAME || true
ssh $HOST -- DISPLAY=:0 xrandr --rmmode $MODENAME || true

# Add a new mode
ssh $HOST -- DISPLAY=:0 xrandr --newmode $MODENAME $MODELINE
ssh $HOST -- DISPLAY=:0 xrandr --addmode $OUTPUT $MODENAME
ssh $HOST -- DISPLAY=:0 xrandr \
  --output $OUTPUT \
  --mode $MODENAME \
  --right-of $PRIMARY \
  --auto

# Spawn a VNC server; tunnel it locally
CLIP=$(
  ssh $HOST -- DISPLAY=:0 xrandr \
    | grep $OUTPUT \
    | awk '{print $3}'
)
ssh -t -L 5900:localhost:5900 $HOST -- x11vnc \
  -localhost \
  -display :0 \
  -viewonly \
  -clip $CLIP \
  -nopw -nocursorshape -nocursorpos -noxinerama \
  -solid &
SSH_PID=$!
sleep 1  # allow time for tunnel to start

# Run a VNC client
systemd-inhibit vinagre --fullscreen  localhost:5900 || true

# Clean up
kill $SSH_PID || true
ssh $HOST -- pkill x11vnc || true
ssh $HOST -- DISPLAY=:0 xrandr --output $OUTPUT --off || true
ssh $HOST -- DISPLAY=:0 xrandr --delmode $OUTPUT $MODENAME || true
ssh $HOST -- DISPLAY=:0 xrandr --rmmode $MODE || true
