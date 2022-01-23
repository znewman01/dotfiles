{ config, pkgs, ... }:

{
  imports = [ ../../home-common.nix ./autorandr.nix ../../code.nix ];
  home.stateVersion = "21.11";
}
