#!/bin/sh
set -e

nix-channel --add https://github.com/rycee/home-manager/archive/master.tar.gz home-manager
nix-channel --update

NIX_PATH=$HOME/.nix-defexpr/channels${NIX_PATH:+:}$NIX_PATH
nix-shell '<home-manager>' -A install

HOME_CONFIG_PATH="$HOME/.config/nixpkgs/home.nix"

if [ -f "$HOME_CONFIG_PATH" ]; then
  mv "$HOME_CONFIG_PATH" "${HOME_CONFIG_PATH}.bak"
fi

ln -s $PWD/home.nix $HOME/.config/nixpkgs/home.nix


home-manager switch

gpg --full-generate-key

find "$HOME/.gnupg/" -type f -exec chmod 600 {} \;
find "$HOME/.gnupg/" -type d -exec chmod 700 {} \;

systemctl --user restart dropbox.service

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


home-manager switch
