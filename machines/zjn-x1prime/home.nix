{ config, pkgs, doom-emacs, ... }:

{
  imports = [
    ../../common/home.nix
    ../../desktop/nixos-home.nix
    # ./autorandr.nix
    ../../code
  ];
  home.stateVersion = "22.11";
}
