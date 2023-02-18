#!/bin/sh
set -x
set -e

if [ ! "$USER" = "root" ]; then
  echo "Must be run as root!"
  exit 1
fi


read -p 'What hostname (usually `zjn-...`)? ' HOSTNAME

nixos-generate-config --root /mnt --show-hardware-config \
  > "machines/$HOSTNAME/hardware-configuration.nix"
nixos-install --flake ".#${HOSTNAME}" --no-root-passwd
