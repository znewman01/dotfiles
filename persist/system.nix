{ config, pkgs, lib, ... }:

{
  programs.fuse.userAllowOther = true;
  boot.initrd.postDeviceCommands = lib.mkAfter ''
    zfs destroy -R tank/encrypt/local/lastroot
    zfs rename tank/encrypt/local/root tank/encrypt/local/lastroot
    zfs set mountpoint=/tank/encrypt/local/lastroot tank/encrypt/local/lastroot
    zfs create -p -o mountpoint=/ tank/encrypt/local/root
  '';
  systemd.tmpfiles.rules = [
    "L /etc/NetworkManager/system-connections - - - - /persist/nm-system-connections"
  ];
  environment.persistence."/persist/root" = {
    directories = [ "/var/lib/tailscale" ];
  };

  systemd.services.initdirs = {
    description = "Set up directories if they don't exist.";
    wantedBy = [ "multi-user.target" ];
    after = [ "persist.mount" "cache.mount" ];
    before = [ "home-manager-zjn.service" "sshd.service" ];
    path = [ "/run/current-system/sw/" ];
    script = ''
      set -eux
      mkdir -p /cache/zjn /persist/zjn /persist/ssh
      chown zjn:users /cache/zjn /persist/zjn
    '';
    serviceConfig = { Type = "oneshot"; };
  };
}
