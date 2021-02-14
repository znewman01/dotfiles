{ config, lib, pkgs, ... }:

let colors = (import ./colors.nix) { lib = lib; };
in {
  programs.alacritty = {
    enable = true;
    settings.font = {
      size = 6;
      family = [
        "Iosevka"
        "Font Awesome 5 Brands"
        "Font Awesome 5 Free"
        "Font Awesome 5 Free Solid"
      ];
      offset.y = 2;
    };
    settings.window.padding = {
      x = 8;
      y = 0;
    };
    settings.colors = {
      primary = {
        background = "0x${colors.base00}";
        foreground = "0x${colors.base05}";
      };
      normal = {
        black = "0x${colors.base00}";
        red = "0x${colors.base08}";
        green = "0x${colors.base0B}";
        yellow = "0x${colors.base0A}";
        blue = "0x${colors.base0D}";
        magenta = "0x${colors.base0E}";
        cyan = "0x${colors.base0C}";
        white = "0x${colors.base05}";
      };
      bright = {
        black = "0x${colors.base03}";
        red = "0x${colors.base08}";
        green = "0x${colors.base0B}";
        yellow = "0x${colors.base0A}";
        blue = "0x${colors.base0D}";
        magenta = "0x${colors.base0E}";
        cyan = "0x${colors.base0C}";
        white = "0x${colors.base07}";
      };
    };
  };
}
