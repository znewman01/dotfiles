{ config, pkgs, ... }:

{
  imports = [ ./autorandr.nix ];
  home.stateVersion = "21.05";
}
