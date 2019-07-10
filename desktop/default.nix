{ config, lib, pkgs, ... }:

{
  imports = [
    ./xmonad.nix
    ./fonts.nix
    ./alacritty.nix
  ];

  home.packages = with pkgs; [
    anki
    i3lock
    xautolock
    xclip
    zathura
  ];

  services.screen-locker = {
    enable = true;
    inactiveInterval = 10;
    lockCmd = "${pkgs.i3lock}/bin/i3lock -n";
  };

}
