{ config, pkgs, doom-emacs, ... }:

{
  imports = [
    ../../common/home-nixos.nix
    ../../desktop/nixos-home.nix
    # ./autorandr.nix
    ../../code
  ];
  home.stateVersion = "22.11";

  services.udiskie = { enable = true; };
}
