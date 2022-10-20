{ pkgs, ... }:

{
  imports = [ ../../home-common.nix ../../code ./autorandr.nix ];
  home.stateVersion = "21.05";
  home.packages = [ pkgs.anki ];
}
