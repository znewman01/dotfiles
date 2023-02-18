{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [ zfs ];

  services.zfs.autoSnapshot.enable = true;
  services.zfs.autoScrub.enable = true;
  systemd.services.zfs-mount.enable = false;
  boot.zfs.enableUnstable = true;

  users.extraGroups.zfs.members = [ "zjn" ];

  virtualisation.docker.storageDriver = "zfs";
}
