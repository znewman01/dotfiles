#!/bin/sh
set -e
set -x

HOST=zjn-workstation
OUTPUT=HDMI-1  # should be unused on the host
PRIMARY=HDMI-3  # primary for the host; we'll be to the right

RESOLUTION=$(xrandr | grep \* | awk '{print $1}')
WIDTH=$(echo $RESOLUTION | sed 's/x.*//')
HEIGHT=$(echo $RESOLUTION | sed 's/.*x//')
FREQ=$(xrandr  | grep \* | awk '{print $2}' | sed 's/\*.*//')
ssh $HOST -- DISPLAY=:0 gtf $WIDTH $HEIGHT $FREQ | grep Modeline
MODELINE=$(ssh $HOST -- gtf $WIDTH $HEIGHT $FREQ | grep Modeline | sed 's/  */ /g' | cut -d' ' -f 4-)
# MODE="${WIDTH}x${HEIGHT}_${FREQ}"
MODE="$(hostname)"
# Ensure cleaned up properly
ssh $HOST -- DISPLAY=:0 xrandr --output $OUTPUT --off
ssh $HOST -- DISPLAY=:0 xrandr --delmode $OUTPUT $MODE || true
ssh $HOST -- DISPLAY=:0 xrandr --rmmode $MODE || true
# Add a new mode
ssh $HOST -- DISPLAY=:0 xrandr --newmode $MODE $MODELINE
ssh $HOST -- DISPLAY=:0 xrandr --addmode $OUTPUT $MODE
ssh $HOST -- DISPLAY=:0 xrandr --output $OUTPUT --mode $MODE --right-of $PRIMARY --auto
CLIP=$(ssh $HOST -- DISPLAY=:0 xrandr | grep $OUTPUT | awk '{print $3}')
# Spawn a VNC server; tunnel it locally
ssh -t -L 5900:localhost:5900 $HOST -- x11vnc -localhost -display :0 -localhost -viewonly -clip $CLIP -nopw -nocursorshape -nocursorpos -noxinerama -solid &
SSH_PID=$!
sleep 5  # allow time for tunnel to start
# Run a VNC client
systemd-inhibit vinagre --fullscreen  localhost:5900 || true
# Clean up
kill $SSH_PID
ssh $HOST -- pkill x11vnc
ssh $HOST -- DISPLAY=:0 xrandr --output $OUTPUT --off
ssh $HOST -- DISPLAY=:0 xrandr --delmode $OUTPUT $MODE || true
ssh $HOST -- DISPLAY=:0 xrandr --rmmode $MODE || true
