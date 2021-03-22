#!/bin/sh
set -e

gpg --full-generate-key

find "$HOME/.gnupg/" -type f -exec chmod 600 {} \;
find "$HOME/.gnupg/" -type d -exec chmod 700 {} \;

dropbox status

read -p "In another terminal, wait until dropbox has actually synced (dropbox status). Then hit [enter]. "

dropbox exclude add "$HOME/Dropbox/Camera Uploads" "$HOME/Dropbox/Photos" "$HOME/Dropbox/Saves"

gpg --output "$HOME/Dropbox/keys/${HOSTNAME}.gpg" --export z@znewman.net

FINGERPRINT=$(gpg --list-keys | grep "Key fingerprint" | head -n 1 | cut -d= -f2 | sed 's/ //g')

read -p "On a viable machine, run:
    $ gpg --import $HOME/Dropbox/keys/${HOSTNAME}.gpg
    $ gpg --edit-key $FINGERPRINT trust quit
    $ xargs pass init $FINGERPRINT < $HOME/.password-store/.gpg-id

Verify that you can now read passwords on this machine. Then hit [enter]. "
