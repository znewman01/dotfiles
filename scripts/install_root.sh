#!/bin/sh
set -x
set -e

if [ ! "$USER" = "root" ]; then
  echo "Must be run as root!"
  exit 1
fi
# MUST be run from dotfiles repo root
# TODO: check/enforce

SYSTEM_WIDE_CONFIG="/etc/nixos/configuration.nix"
NORMAL_USER=zjn
NORMAL_GROUP=users

HOSTID=$(head -c 8 /etc/machine-id)
STATEVERSION=$(nix eval -f '<nixpkgs/nixos>' 'config.system.stateVersion' | tr -d '"')

for template_file in $(find . -name '*.template'); do
  f="${template_file%.template}"
  cp "$template_file" "$f"
  sed -i -e "s'{{HOSTNAME}}'${HOSTNAME}'g" "$f"
  sed -i -e "s'{{HOSTID}}'${HOSTID}'g" "$f"
  sed -i -e "s'{{STATEVERSION}}'${STATEVERSION}'g"  "$f"
  # TODO: delete extra files
done

rsync -riub machines/template/ "machines/${HOSTNAME}"
rm "machines/${HOSTNAME}/"*.template
for template_file in machines/template/*.template; do
  rm "${template_file%.template}"
done

if [ -f /etc/nixos/configuration.nix ]; then
  mv "$SYSTEM_WIDE_CONFIG" "${SYSTEM_WIDE_CONFIG}.bak"
fi

ln -s ${PWD}/configuration.nix /etc/nixos/configuration.nix
chown -R "$NORMAL_USER:$NORMAL_GROUP" *

nixos-rebuild --upgrade boot
