
{ config, pkgs, ... }:

{
  imports = [ ./default.nix ];

  home.packages = with pkgs; [
    skypeforlinux
    signal-desktop
    keybase-gui
    slack
    zoom-us
  ];
}
