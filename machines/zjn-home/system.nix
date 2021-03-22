{ config, pkgs, lib, ... }:

{
  networking.hostName = "zjn-home";
  networking.hostId = "2f4cf0b0";
  system.stateVersion = "20.09";
  boot.kernelPackages = pkgs.linuxPackages_5_10;
  boot.initrd.postDeviceCommands = lib.mkAfter ''
    zfs rollback -r tank/local/root@blank
  '';
  systemd.tmpfiles.rules = [
    "L /etc/nixos/configuration.nix - - - - /persist/zjn/git/dotfiles/configuration.nix"
    "L /etc/nixos/hardware-configuration.nix - - - - /persist/hardware-configuration.nix"
  ];
}
