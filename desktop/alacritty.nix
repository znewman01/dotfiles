{ config, lib, pkgs, ... }:

let
  # TODO: make module
  bgColor = "0x282a36";
  fgColor = "0xf8f8f2";
in
{
  programs.alacritty = {
    enable = true;
    settings.font.size = 7;
    settings.font.family = [ "Hack" "FontAwesome" ];
    settings.colors = {
      primary = {
        background = "${bgColor}";
        foreground = "${fgColor}";
      };
      normal = {
        black = "0x000000";
        red = "0xff5555";
        green = "0x50fa7b";
        yellow = "0xf1fa8c";
        blue = "0xcaa9fa";
        magenta = "0xff79c6";
        cyan = "0x8be9fd";
        white = "0xbfbfbf";
      };
      bright = {
        black = "0x575b70";
        red = "0xff6e67";
        green = "0x5af78e";
        yellow = "0xf4f99d";
        blue = "0xcaa9fa";
        magenta = "0xff92d0";
        cyan = "0x9aedfe";
        white = "0xe6e6e6";
      };
    };
  };
}