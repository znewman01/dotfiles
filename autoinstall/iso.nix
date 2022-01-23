{ config, pkgs, lib, ... }:

let
  keys =
    lib.strings.concatStrings (map builtins.readFile (import ../net/keys.nix));
in {
  systemd.services.install = {
    description = "Bootstrap a NixOS installation";
    wantedBy = [ "multi-user.target" ];
    after = [ "network.target" "polkit.service" ];
    path = [ "/run/current-system/sw/" ];
    script = with pkgs; ''
      set -eux


      # 0. Preliminaries
      echo 'journalctl -fb -n100 -uinstall' >>~nixos/.bash_history
      DISK=/dev/nvme0n1
      PART_PREFIX=/dev/nvme0n1p

      # 1. Partition
      parted --script $DISK -- mklabel GPT
      parted --script $DISK -- mkpart primary 512MiB 100%
      parted --script $DISK -- mkpart ESP fat32 1MiB 512MiB
      parted --script $DISK -- set 2 esp on


      # 2. Format
      mkfs.fat -F 32 -n boot ''${PART_PREFIX}2
      zpool create -f -O compression=lz4 -O com.sun:auto-snapshot=true tank ''${PART_PREFIX}1
      zfs create -p -o mountpoint=none tank/safe
      zfs create -p -o mountpoint=legacy tank/safe/persist
      zfs create -p -o mountpoint=none tank/local
      zfs create -p -o mountpoint=none tank/backups
      zfs create -p -o mountpoint=legacy tank/local/root
      zfs create -p -o mountpoint=legacy tank/local/cache
      zfs create -p -o mountpoint=legacy tank/local/nix
      sync


      # 3. Mount
      mount -t zfs tank/local/root /mnt
      mkdir /mnt/{boot,cache,persist,nix}
      mount -t zfs tank/local/nix /mnt/nix
      mount -t zfs tank/local/cache /mnt/cache
      mount -t zfs tank/safe/persist /mnt/persist
      mount ''${PART_PREFIX}2 /mnt/boot


      # 4. Install configurations
      install -D ${./configuration.nix} /mnt/etc/nixos/configuration.nix
      install -D ${
        ./hardware-configuration.nix
      } /mnt/etc/nixos/hardware-configuration.nix

      for dst in {/mnt,}/etc/ssh/authorized_keys_install; do
        install -D ${writeText "authorized_keys" keys} $dst
      done

      sed -i -E 's/(\w*)#installer-only /\1/' /mnt/etc/nixos/*
      hostid="$(printf "00000000%x" "$(cksum /etc/machine-id | cut -d' ' -f1)" | tail -c8)"
      sed -i -E 's/babecafe/'$hostid'/' /mnt/etc/nixos/hardware-configuration.nix
      sed -i -E 's~/dev/null~/etc/ssh/authorized_keys_install~' /mnt/etc/nixos/hardware-configuration.nix


      # 5. Do the install
      ${config.system.build.nixos-install}/bin/nixos-install \
        --system ${
          (import <nixpkgs/nixos/lib/eval-config.nix> {
            system = "x86_64-linux";
            modules = [ ./configuration.nix ./hardware-configuration.nix ];
          }).config.system.build.toplevel
        } \
        --no-root-passwd \
        --cores 0

      echo 'Shutting off...'
      ${systemd}/bin/shutdown now
    '';
    environment = config.nix.envVars // {
      inherit (config.environment.sessionVariables) NIX_PATH;
      HOME = "/root";
    };
    serviceConfig = { Type = "oneshot"; };
  };
}
