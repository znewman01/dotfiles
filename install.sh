#!/bin/sh

if [ ! -f /etc/nixos/configuration.nix ]; then
  ln -s ${PWD}/configuration.nix /etc/nixos/configuration.nix
fi

nix-channel --add https://github.com/rycee/home-manager/archive/master.tar.gz home-manager
nix-channel --update

NIX_PATH=$HOME/.nix-defexpr/channels${NIX_PATH:+:}$NIX_PATH
nix-shell '<home-manager>' -A install

if [ ! -f $HOME/.config/nixpkgs/home.nix ]; then
  ln -s $PWD/home.nix $HOME/.config/nixpkgs/home.nix
fi
