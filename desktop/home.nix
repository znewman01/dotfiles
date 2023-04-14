{ config, pkgs, lib, ... }:

{
  imports =
    [ ./tools ./gpg-home.nix ./web/firefox ../emacs ./syncthing/home.nix ];

  colorScheme = lib.optionalAttrs pkgs.stdenv.isLinux {
    enable = true;
    name = "solarized";
  };
  fonts.enable = pkgs.stdenv.isLinux;
  menus.enable = pkgs.stdenv.isLinux;
  notifications.enable = pkgs.stdenv.isLinux;
  terminal.enable = pkgs.stdenv.isLinux;
  screenlock.enable = pkgs.stdenv.isLinux;
  programs.pass = {
    enable = true;
    store = if pkgs.stdenv.isDarwin then
      "/Users/zjn/Sync/passwords"
    else
      "/home/zjn/Sync/passwords";
  };
  programs.home-manager.enable = true;
}

