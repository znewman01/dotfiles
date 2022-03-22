{ config, pkgs, ... }:

{
  imports = [
    ./pass.nix
    # ./tex.nix
  ];

  programs.home-manager.enable = true;
}

