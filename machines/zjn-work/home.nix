{ config, pkgs, doom-emacs, ... }:

{
  imports = [
    ../../common/home-nixos.nix
    ../../desktop/nixos-home.nix
    ./autorandr.nix
    ../../code
  ];
  home.stateVersion = "21.11";
}
