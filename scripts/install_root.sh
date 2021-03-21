#!/bin/sh
set -x
set -e

# MUST be run from dotfiles repo root
# TODO: check/enforce

SYSTEM_WIDE_CONFIG="/etc/nixos/configuration.nix"

HOSTID=$(head -c 8 /etc/machine-id)
STATEVERSION=$(nix eval -f '<nixpkgs/nixos>' 'config.system.stateVersion')

cp -r hosts/machines/template "hosts/${HOSTNAME}"

for template_file in $(find . -name '*.template'); do
  cat "$template_file" \
    | sed -e "s'{{HOSTNAME}}'${HOSTNAME}'g" \
    | sed -e "s'{{HOSTID}}'${HOSTID}'g" \
    | sed -e "s'{{STATEVERSION}}'${STATEVERSION}'g" \
    > "${template_file%.template}"
  # TODO: delete extra files
done

if [ -f /etc/nixos/configuration.nix ]; then
  mv "$SYSTEM_WIDE_CONFIG" "${SYSTEM_WIDE_CONFIG}.bak"
fi

ln -s ${PWD}/configuration.nix /etc/nixos/configuration.nix

nixos-rebuild --upgrade boot
