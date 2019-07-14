#!/bin/sh
set -x
set -e

SYSTEM_WIDE_CONFIG="/etc/nixos/configuration.nix"

if [ -f /etc/nixos/configuration.nix ]; then
  mv "$SYSTEM_WIDE_CONFIG" "${SYSTEM_WIDE_CONFIG}.bak"
fi

ln -s ${PWD}/configuration.nix /etc/nixos/configuration.nix

nix-channel --add https://nixos.org/channels/nixos-unstable nixos

nixos-rebuild --upgrade boot
