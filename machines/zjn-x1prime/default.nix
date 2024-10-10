{ config, pkgs, lib, impermanence, ... }:

{
  imports = [
    ../../common/nixos.nix
    ../../desktop/nixos.nix
    ./hardware-configuration.nix
  ];
  home-manager.users.zjn.imports = [ ./home.nix ];
  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = true;

  networking.hostName = "zjn-x1prime";
  networking.hostId = "e2102672";
  networking.networkmanager.enable = true;
  system.stateVersion = "22.11";

  # For Thinkpad firmware.
  services.fwupd.enable = true;
}
