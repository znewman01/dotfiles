{ config, pkgs, lib, ... }:

let colors = (import ./colors) { lib = lib; };
in {
  home.packages = with pkgs; [
    i3lock
    xautolock
  ];

  services.screen-locker = {
    enable = true;
    inactiveInterval = 10;
    lockCmd = "${pkgs.i3lock}/bin/i3lock -n -c ${colors.base02}";
  };
}

