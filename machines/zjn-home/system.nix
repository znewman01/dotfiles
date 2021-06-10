{ config, pkgs, lib, ... }:

{
  networking.hostName = "zjn-home";
  networking.hostId = "2f4cf0b0";
  system.stateVersion = "20.09";
  programs.fuse.userAllowOther = true;
  boot.initrd.postDeviceCommands = lib.mkAfter ''
    zfs destroy -R tank/local/lastroot
    zfs rename tank/local/root tank/local/lastroot
    zfs set mountpoint=/tank/local/lastroot tank/local/lastroot
    zfs create -p -o mountpoint=legacy tank/local/root
  '';
  services.zfs.autoSnapshot.enable = true;
  services.openssh = {
    hostKeys = [{
      path = "/persist/ssh/ssh_host_ed25519_key";
      type = "ed25519";
    }];
  };
  systemd.tmpfiles.rules = [
    "L /etc/nixos/configuration.nix - - - - /persist/zjn/git/dotfiles/configuration.nix"
    "L /etc/nixos/hardware-configuration.nix - - - - /persist/hardware-configuration.nix"
  ];
}
