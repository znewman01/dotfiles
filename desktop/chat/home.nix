{ config, pkgs, lib, ... }:

{
  home.packages = [ pkgs.element-desktop ] ++ lib.optionals pkgs.stdenv.isLinux
    (with pkgs; [ skypeforlinux signal-desktop keybase-gui slack zoom-us ]);
}
