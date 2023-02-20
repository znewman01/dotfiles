{ config, lib, pkgs, ... }:

with lib;

let cfg = config.terminal;
in {
  options.terminal = {
    enable = mkOption {
      type = types.bool;
      description = "Whether to enable terminal management.";
      default = false;
    };
    defaultCommand = mkOption {
      type = types.str;
      description = "Default command to spawn a new terminal.";
      default = "alacritty";
    };
    spawn = mkOption {
      type = types.anything;
      description = "Function";
      default = windowTitle: command:
        "alacritty --title '${windowTitle}' --command ${command}";
    };
  };

  config = mkIf cfg.enable {
    programs.alacritty = {
      enable = true;
      settings.font = {
        size = (builtins.head config.fonts.terminalFonts).size;
        family = builtins.map (font: font.name) config.fonts.terminalFonts;
        offset.y = 2;
      };
      settings.window.padding = {
        x = 8;
        y = 0;
      };
      settings.colors = let colors = config.colorScheme.colors;
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
  };
}
