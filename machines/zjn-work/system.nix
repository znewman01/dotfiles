{ config, pkgs, lib, ... }:

{
  imports = [
    ../../common.nix
    ./hardware-configuration.nix
    ../../desktop/work.nix
  ];

  networking.hostName = "zjn-work";
  system.stateVersion = "21.11";
}
