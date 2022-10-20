{ pkgs, lib, ... }: {
  programs.alacritty = lib.optionalAttrs pkgs.stdenv.isLinux {
    enable = true;
    settings.font = {
      size = 12;
      family = [
        "Roboto Mono" # TODO: put the fonts in one place
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
    settings.colors = let colors = (import ./colors) { inherit lib; };
    in {
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
    # https://github.com/alacritty/alacritty/issues/1501
    # Use pxls, not pts for font
    settings.env = { "WINIT_X11_SCALE_FACTOR" = "1.0"; };
  };
}
