#!/bin/sh
set -x
set -e

SYSTEM_WIDE_CONFIG="/etc/nixos/configuration.nix"

read -p "Hostname for this machine? " DESIRED_HOSTNAME

sed -e "s'{{HOSTNAME}}'${DESIRED_HOSTNAME}'g" \
    configuration.nix.template > configuration.nix

if [ -f /etc/nixos/configuration.nix ]; then
  mv "$SYSTEM_WIDE_CONFIG" "${SYSTEM_WIDE_CONFIG}.bak"
fi

ln -s ${PWD}/configuration.nix /etc/nixos/configuration.nix

nixos-rebuild --upgrade boot
