{ config, nixpkgs, lib, impermanence, ... }:

{
  imports = [
    ../../common/nixos.nix
    ../../desktop/nixos.nix
    ./hardware-configuration.nix
    ../../work/default.nix
  ];
  home-manager.users.zjn.imports = [ ./home.nix ];

  networking.hostName = "zjn-work";
  networking.hostId = "ec47a9c8";

  system.stateVersion = "21.11";
}
