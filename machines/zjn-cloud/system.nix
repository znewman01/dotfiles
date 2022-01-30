{ config, pkgs, lib, ... }:

{
  imports = [ ../../common.nix ./hardware-configuration.nix ];

  networking.hostName = "zjn-cloud";
  system.stateVersion = "21.11";

}
