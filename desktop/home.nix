{ config, pkgs, ... }:

{
  imports = [
    ./tools
    ./pass.nix
    ./gpg-home.nix
    ./web/home.nix
    ../emacs
    ./syncthing/home.nix
    ./terminals-home.nix
  ];

  programs.home-manager.enable = true;
}

