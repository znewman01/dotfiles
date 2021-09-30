{ config, pkgs, ... }:

{
  imports = [ ./autorandr.nix ../../services/lichess.nix ];
  home.stateVersion = "21.05";
}
