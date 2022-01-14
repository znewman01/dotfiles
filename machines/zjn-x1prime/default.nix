{ config, pkgs, ... }:

{
  imports = [ ./autorandr.nix ./../../email ./../../desktop/chat ];
}
