{ config, pkgs, ... }:

{
  imports = [
    ./pass.nix
    ./tex.nix
    ./tools
  ];

  programs.home-manager.enable = true;
}

