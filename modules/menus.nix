{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.menus;
  colors = config.colorScheme.colors;
in {
  options.menus = {
    enable = mkOption {
      type = types.bool;
      description = "Whether to enable menu management.";
      default = false;
    };
    program = mkOption {
      type = types.enum [ "rofi" ];
      description = "Driver for menus.";
      default = "rofi";
    };
    runCommand = mkOption {
      type = types.str;
      description = "Command to spawn a program.";
      example = "dmenu_run";
    };
    passwordCommand = mkOption {
      type = types.str;
      description = "Command to type a password.";
    };
    calcCommand = mkOption {
      type = types.str;
      description = "Command to calculator..";
    };
  };

  config = mkIf cfg.enable {
    programs.rofi = {
      enable = true;
      theme = let
        colors = config.colorScheme.colors;
        hexDigitToDec = (flip builtins.getAttr) {
          "0" = 0;
          "1" = 1;
          "2" = 2;
          "3" = 3;
          "4" = 4;
          "5" = 5;
          "6" = 6;
          "7" = 7;
          "8" = 8;
          "9" = 9;
          "a" = 10;
          "b" = 11;
          "c" = 12;
          "d" = 13;
          "e" = 14;
          "f" = 15;
        };
        hexPairToDec = hex:
          16 * (hexDigitToDec (builtins.substring 0 1 hex))
          + (hexDigitToDec (builtins.substring 1 1 hex));
        toRGBA = hexString: opacity:
          let
            r = builtins.toString
              (hexPairToDec (builtins.substring 0 2 hexString));
            g = builtins.toString
              (hexPairToDec (builtins.substring 2 2 hexString));
            b = builtins.toString
              (hexPairToDec (builtins.substring 4 2 hexString));
          in "rgba ( ${r}, ${g}, ${b}, ${builtins.toString opacity} %)";
        toRGB = hexString: toRGBA hexString 100;
        themeConfig = pkgs.writeText "config.rasi" ''
          * {
              red: ${toRGB colors.base08};
              blue: ${toRGB colors.base0D};
              lightfg: ${toRGB colors.base04};
              lightbg: ${toRGB colors.base01};
              foreground: ${toRGB colors.base04};
              background: ${toRGB colors.base00};
              background-color: ${toRGBA colors.base00 0};
              separatorcolor:              @foreground;
              border-color:                @foreground;
              selected-normal-foreground:  @lightbg;
              selected-normal-background:  @lightfg;
              selected-active-foreground:  @background;
              selected-active-background:  @blue;
              selected-urgent-foreground:  @background;
              selected-urgent-background:  @red;
              normal-foreground:           @foreground;
              normal-background:           @background;
              active-foreground:           @blue;
              active-background:           @background;
              urgent-foreground:           @red;
              urgent-background:           @background;
              alternate-normal-foreground: @foreground;
              alternate-normal-background: @lightbg;
              alternate-active-foreground: @blue;
              alternate-active-background: @lightbg;
              alternate-urgent-foreground: @red;
              alternate-urgent-background: @lightbg;
              spacing:                     2;
          }
          window {
              background-color: @background;
              border:           1;
              padding:          5;
          }
          mainbox {
              border:           0;
              padding:          0;
          }
          message {
              border:           1px solid 0px 0px ;
              border-color:     @separatorcolor;
              padding:          1px ;
          }
          textbox {
              text-color:       @foreground;
          }
          listview {
              fixed-height:     0;
              border:           2px solid 0px 0px ;
              border-color:     @separatorcolor;
              spacing:          2px ;
              scrollbar:        true;
              padding:          2px 0px 0px ;
          }
          element {
              border:           0;
              padding:          1px ;
          }
          element normal.normal {
              background-color: @normal-background;
              text-color:       @normal-foreground;
          }
          element normal.urgent {
              background-color: @urgent-background;
              text-color:       @urgent-foreground;
          }
          element normal.active {
              background-color: @active-background;
              text-color:       @active-foreground;
          }
          element selected.normal {
              background-color: @selected-normal-background;
              text-color:       @selected-normal-foreground;
          }
          element selected.urgent {
              background-color: @selected-urgent-background;
              text-color:       @selected-urgent-foreground;
          }
          element selected.active {
              background-color: @selected-active-background;
              text-color:       @selected-active-foreground;
          }
          element alternate.normal {
              background-color: @alternate-normal-background;
              text-color:       @alternate-normal-foreground;
          }
          element alternate.urgent {
              background-color: @alternate-urgent-background;
              text-color:       @alternate-urgent-foreground;
          }
          element alternate.active {
              background-color: @alternate-active-background;
              text-color:       @alternate-active-foreground;
          }
          scrollbar {
              width:            4px ;
              border:           0;
              handle-color:     @normal-foreground;
              handle-width:     8px ;
              padding:          0;
          }
          sidebar {
              border:           2px solid 0px 0px ;
              border-color:     @separatorcolor;
          }
          button {
              spacing:          0;
              text-color:       @normal-foreground;
          }
          button selected {
              background-color: @selected-normal-background;
              text-color:       @selected-normal-foreground;
          }
          inputbar {
              spacing:          0px;
              text-color:       @normal-foreground;
              padding:          1px ;
              children:         [ prompt,textbox-prompt-colon,entry,case-indicator ];
          }
          case-indicator {
              spacing:          0;
              text-color:       @normal-foreground;
          }
          entry {
              spacing:          0;
              text-color:       @normal-foreground;
          }
          prompt {
              spacing:          0;
              text-color:       @normal-foreground;
          }
          textbox-prompt-colon {
              expand:           false;
              str:              ":";
              margin:           0px 0.3000em 0.0000em 0.0000em ;
              text-color:       inherit;
          }
        '';
      in "${themeConfig}";
      package = with pkgs; rofi.override { plugins = [ rofi-calc rofi-pass ]; };
      pass = {
        enable = config.programs.pass.enable;
        stores = [ config.programs.pass.store ];
        extraConfig = ''
          USERNAME_field='user'
          AUTOTYPE_field='autotype'
          BROWSER='xdg-open'
          EDITOR='em-float'
          default_do='autotype'
          copy_pass='Ctrl+c'
          copy_menu="Alt+C"
          type_pass='Ctrl+t'
          type_menu="Ctrl+T"
          default_user='znewman01'
          open_url='Ctrl+o'
          action_menu='Ctrl+A'
          insert_pass='Ctrl+i'
          show='Ctrl+s'
          clip=clipboard
          clip_clear=45
          help_color='#4872FF'  # "#4872FF"; # https://github.com/carnager/rofi-pass/issues/226
        '';
      };
      font = let font = builtins.head config.fonts.terminalFonts;
      in "${font.name} ${builtins.toString (builtins.floor font.size)}";
      extraConfig = { m = "-4"; };
    };

    menus.runCommand = "rofi -show run";
    menus.passwordCommand = "rofi-pass";
    menus.calcCommand =
      "rofi -show calc -modi calc -no-show-match -no-sort -lines 0 -calc-command \"${pkgs.xdotool}/bin/xdotool type '{result}'\" -kb-accept-custom 'Return' -kb-accept-entry ''";
  };
}
