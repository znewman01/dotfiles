{ config, pkgs, doom-emacs, ... }:

{
  imports = [
    ../../common/home-nixos.nix
    ../../desktop/nixos-home.nix
    # ./autorandr.nix
    # ../../code
    # ../../work/home.nix
  ];
  home.stateVersion = "22.11";
}
