{ config, lib, pkgs, ... }:

with lib;

let cfg = config.fonts;
in {
  options.fonts = {
    enable = mkOption {
      type = types.bool;
      description = "Enable font management.";
      example = true;
      default = false;
    };

    terminalFonts = mkOption {
      type = types.listOf hm.types.fontType;
      description = "TODO";
      default = [
        {
          name = "Roboto Mono";
          package = pkgs.roboto-mono;
          size = 12.0;
        }
        {
          name = "RobotoMono Nerd Font";
          package = pkgs.nerdfonts.override { fonts = [ "RobotoMono" ]; };
          size = 15.0;
        }
      ];
    };
    extraFontPackages = mkOption {
      type = types.listOf types.package;
      description = "Extra font packages to have available";
      default = with pkgs; [
        corefonts
        source-code-pro
        hack-font
        eb-garamond
        fira-code
        iosevka
        liberation_ttf
        libertine
        libertinus
        noto-fonts
        noto-fonts-emoji
        noto-fonts-extra
        ttf_bitstream_vera
      ];
    };
  };

  config = mkIf cfg.enable {
    home.packages = cfg.extraFontPackages ++ (builtins.filter (x: x != null)
      (builtins.map (font: font.package) cfg.terminalFonts));
  };
}
