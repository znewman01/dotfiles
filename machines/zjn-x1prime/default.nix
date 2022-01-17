{ config, pkgs, ... }:

{
  imports = [ ./autorandr.nix ./../../email ./../../desktop/chat.nix ];
  home.stateVersion = "21.05";
}
