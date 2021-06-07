#!/bin/sh
set -e

gpg --full-generate-key

find "$HOME/.gnupg/" -type f -exec chmod 600 {} \;
find "$HOME/.gnupg/" -type d -exec chmod 700 {} \;

read -p "Hook up sync thing, wait for it to finish, hit enter."

# TODO: setup/copy ssh pubkey here
gpg --output "$HOME/Sync/keys/${HOSTNAME}.gpg" --export z@znewman.net

FINGERPRINT=$(gpg --list-keys | grep "Key fingerprint" | head -n 1 | cut -d= -f2 | sed 's/ //g')

read -p "On a viable machine, run:
    $ gpg --import $HOME/Sync/keys/${HOSTNAME}.gpg
    $ echo '$FINGERPRINT:6' | gpg --import-ownertrust
    $ xargs pass init $FINGERPRINT < $HOME/.password-store/.gpg-id

Verify that you can now read passwords on this machine. Then hit [enter]. "
