ssh -t -L 5900:localhost:5900 128.30.93.231 'x11vnc -localhost -display :0 -localhost -viewonly -clip 1280x1024+1280+0 -nopw -nocursorshape -nocursorpos -noxinerama -solid'
remmina -c vnc://localhost:5900
