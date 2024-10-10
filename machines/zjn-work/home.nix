{ config, pkgs, ... }:

{
  imports = [
    ../../common/home-nixos.nix
    ../../desktop/nixos-home.nix
    ./autorandr.nix
    ../../code
    ../../work/home.nix
  ];
  home.stateVersion = "21.11";
}
