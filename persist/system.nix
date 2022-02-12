{ config, pkgs, lib, ... }:

{
  programs.fuse.userAllowOther = true;
  boot.initrd.postDeviceCommands = lib.mkAfter ''
    zfs destroy -R tank/local/lastroot
    zfs rename tank/local/root tank/local/lastroot
    zfs set mountpoint=/tank/local/lastroot tank/local/lastroot
    zfs create -p -o mountpoint=legacy tank/local/root
  '';
  services.openssh = {
    hostKeys = [{
      path = "/persist/ssh/ssh_host_ed25519_key";
      type = "ed25519";
    }];
  };
  systemd.tmpfiles.rules = [
    "L /etc/NetworkManager/system-connections - - - - /persist/nm-system-connections"
    # Needed for znapzend (so can SSH to other machines)
    "C /root/.ssh - - - - /persist/zjn/.ssh"
    "z /root/.ssh - root root - -"
  ];
  environment.persistence."/persist/root" = {
    directories = [ "/var/lib/acme" "/var/lib/tailscale" ];
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
