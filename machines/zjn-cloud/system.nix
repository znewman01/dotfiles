{ config, pkgs, lib, ... }:

{
  imports = [
    ../../common.nix
    ./hardware-configuration.nix
    ../../services/syncthing.nix
  ];

  networking.hostName = "zjn-cloud";
  system.stateVersion = "21.11";
  networking.hostId = "29b702d1";

  systemd.services.formatdisk = {
    description = "Format backup disk";
    wantedBy = [ "multi-user.target" ];
    before = [ "home-manager-zjn.service" "sshd.service" ];
    path = [ "/run/current-system/sw/" ];
    script = ''
      DISK=/dev/disk/by-id/scsi-0Google_PersistentDisk_backups-part1
      if blkid -o export $DISK | grep zfs; then
        echo "Disk already has ZFS format!"
      else
        zpool create -f -O compression=lz4 -O com.sun:auto-snapshot=true tank $DISK
        zfs create -p -o mountpoint=none tank/backups
        zfs create -p -o mountpoint=legacy tank/syncthing
      fi
    '';
    serviceConfig = { Type = "oneshot"; };
  };
}
