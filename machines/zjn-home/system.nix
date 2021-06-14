{ config, pkgs, lib, ... }:

{
  # ZFS broken in 5.12
  boot.kernelPackages = pkgs.linuxPackages_5_10;

  networking.hostName = "zjn-home";
  networking.hostId = "2f4cf0b0";
  system.stateVersion = "20.09";
}
