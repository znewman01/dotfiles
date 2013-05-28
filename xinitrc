#!/bin/sh

[ -f /etc/xprofile ] && . /etc/xprofile
[ -f ~/.xprofile ] && . ~/.xprofile

setxkbmap -option caps:swapescape
setxkbmap -option compose:ralt

dropboxd&
xautolock -time 5 -locker i3lock&
exec ssh-agent wmii
