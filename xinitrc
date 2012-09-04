#!/bin/sh

[ -f /etc/xprofile ] && . /etc/xprofile
[ -f ~/.xprofile ] && . ~/.xprofile

setxkbmap -option caps:swapescape

dropboxd&
xautolock -time 5 -locker i3lock&
exec ssh-agent wmii
