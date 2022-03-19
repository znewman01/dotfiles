
{ config, pkgs, ... }:

{
  imports = [ ./default.nix ];

  home.packages = with pkgs; [
    skype
    signal-desktop
    keybase-gui
    slack
    zoom-us
  ];
}
