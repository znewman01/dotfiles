{ config, pkgs, lib, ... }:

{
  imports = [ ./hardware-configuration.nix ./../../desktop/work.nix ];

  boot.kernelPackages = pkgs.linuxPackages_5_10;

  networking.hostName = "zjn-home";
  system.stateVersion = "20.09";
}
