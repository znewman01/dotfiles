{ config, pkgs, ... }:

{
  imports = [ ./autorandr.nix ../../services/lichess.nix ];
}
