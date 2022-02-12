{ config, pkgs, lib, ... }:

{
  imports = [
    ../../common.nix
    ../../desktop.nix
    ./hardware-configuration.nix
    ../../desktop/work.nix
  ];
  home-manager.users.zjn.imports = [ ./home.nix ];

  networking.hostName = "zjn-work";
  system.stateVersion = "21.11";
}
