{ config, pkgs, ... }:

{
  imports = [ ./pass.nix ./gpg-home.nix ];

  programs.home-manager.enable = true;
}

