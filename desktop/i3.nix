{ config, pkgs, lib, ... }:

let
  colors =
    builtins.mapAttrs (name: value: "#${value}") config.colorScheme.colors;
  fonts = {
    names = builtins.map (font: font.name) config.fonts.terminalFonts;
    size = (builtins.head config.fonts.terminalFonts).size;
  };
in {
  programs.i3status = {
    enable = true;
    enableDefault = false;
    general = {
      output_format = "i3bar";
      colors = true;
      color_good = colors.base0B;
      color_degraded = colors.base0A;
      color_bad = colors.base08;
      color_separator = colors.base07;
      separator = "|";
    };
    modules = {
      "wireless _first_" = {
        position = 1;
        settings = {
          format_up = "WIFI%quality <%essid> (%ip)";
          format_down = "󰖪";
          format_quality = "%d";
        };
      };
      "ethernet _first_" = {
        position = 2;
        settings = {
          format_up = "󰈁 %ip (%speed)";
          format_down = "󰈂";
        };
      };
      "battery all" = {
        position = 4;
        settings = {
          format = "%statusBAT%percentageBAT";
          status_chr = "󱐋";
          status_bat = "";
          low_threshold = "30";
        };
      };
      "memory" = {
        position = 5;
        settings = {
          format = "󰍛 MEM%percentage_usedMEM";
          threshold_degraded = "1G";
        };
      };
      "tztime local" = {
        position = 6;
        settings = { format = " %Y-%m-%d 󰥔 %H:%M:%S "; };
      };
    };
  };
  home.packages = with pkgs; [ dmenu ];
  xsession = {
    enable = true;
    windowManager.i3 = {
      enable = true;
      config = {
        terminal = config.terminal.defaultCommand;
        keycodebindings = {
          "232" = ''exec "light -U 10"''; # XF86MonBrightnessDown
          "233" = ''exec "light -A 10"''; # XF86MonBrightnessUp
        };
        keybindings =
          let modifier = config.xsession.windowManager.i3.config.modifier;
          in lib.mkOptionDefault ({
            # Normal
            "${modifier}+h" = "focus left";
            "${modifier}+j" = "focus down";
            "${modifier}+k" = "focus up";
            "${modifier}+l" = "focus right";
            "${modifier}+Shift+h" = "move left";
            "${modifier}+Shift+j" = "move down";
            "${modifier}+Shift+k" = "move up";
            "${modifier}+Shift+l" = "move right";
            "${modifier}+space" = "layout toggle split";
            "${modifier}+Shift+d" = "kill";
            # Scratchpad
            "${modifier}+semicolon" = "scratchpad show";
            "${modifier}+Shift+semicolon" = "move scratchpad";
            # Screenshots
            "${modifier}+Shift+r" =
              "exec --no-startup-id ${./record_screen.sh}";
            "${modifier}+Control+Shift+r" = ''
              exec --no-startup-id "ARG=$(echo -e 'selection\nwindow\nall\nfullscreen' | rofi -dmenu -no-custom -p 'Record') && record_screen.sh $ARG"'';
            "Print" = ''exec "${./screenshot.sh} -s"'';
            "${modifier}+Print" = ''exec "${./screenshot.sh} fullscreen"'';
          } // (lib.optionalAttrs true {
            "${modifier}+Shift+e" =
              ''exec --no-startup-id emacsclient --eval "(emacs-everywhere)"'';
            "${modifier}+Shift+x" = "exec org-capture";
          }) // (lib.optionalAttrs config.terminal.enable {
            "${modifier}+Shift+n" = let
              cmd = config.terminal.spawn "nixos-rebuild"
                "/bin/sh -c 'sudo nixos-rebuild switch --flake ~/git/dotfiles; notify-send -t 2000 done; echo nixos-rebuild done.; read'";
            in ''exec "${cmd}"'';
          }) // (lib.optionalAttrs config.screenlock.enable {
            "Control+Shift+l" = ''exec "${config.screenlock.lockCommand}"'';
          }) // (lib.optionalAttrs config.menus.enable {
            "${modifier}+p" = ''exec "${config.menus.runCommand}"'';
            "${modifier}+Shift+p" = "exec ${config.menus.passwordCommand}";
            "${modifier}+Shift+c" = "exec ${config.menus.calcCommand}";
          }));
        modes.resize = {
          "h" = "resize shrink width 5 px or 5 ppt";
          "j" = "resize shrink height 5 px or 5 ppt";
          "k" = "resize grow height 5 px or 5 ppt";
          "l" = "resize grow width 5 px or 5 ppt";
          "Escape" = "mode default";
          "Return" = "mode default";
        };
        floating.criteria = [
          { title = "doom-capture"; }
          { title = "nixos-rebuild"; }
          { instance = "emacs-everywhere"; }
        ];
        inherit fonts;
        # From https://github.com/tinted-theming/base16-i3/blob/master/templates/default.mustache
        colors = {
          focused = {
            border = colors.base05;
            background = colors.base05;
            text = colors.base00;
            indicator = colors.base05;
            childBorder = colors.base05;
          };
          focusedInactive = {
            border = colors.base01;
            background = colors.base01;
            text = colors.base05;
            indicator = colors.base01;
            childBorder = colors.base01;
          };
          unfocused = {
            border = colors.base01;
            background = colors.base00;
            text = colors.base05;
            indicator = colors.base01;
            childBorder = colors.base01;
          };
          urgent = {
            border = colors.base08;
            background = colors.base08;
            text = colors.base00;
            indicator = colors.base08;
            childBorder = colors.base08;
          };
          placeholder = {
            border = colors.base00;
            background = colors.base00;
            text = colors.base05;
            indicator = colors.base00;
            childBorder = colors.base00;
          };
          background = colors.base07;
        };
        bars = [{
          inherit fonts;
          extraConfig = ''
            separator_symbol " | "
          '';
          colors = {
            background = colors.base00;
            separator = colors.base02;
            statusline = colors.base04;
            focusedWorkspace = {
              border = colors.base05;
              background = colors.base0D;
              text = colors.base00;
            };
            activeWorkspace = {
              border = colors.base05;
              background = colors.base03;
              text = colors.base00;
            };
            inactiveWorkspace = {
              border = colors.base03;
              background = colors.base01;
              text = colors.base05;
            };
            urgentWorkspace = {
              border = colors.base08;
              background = colors.base08;
              text = colors.base00;
            };
            bindingMode = {
              border = colors.base00;
              background = colors.base0A;
              text = colors.base00;
            };
          };
          statusCommand = let
            wrapper = pkgs.writeScript "i3statuswrapper.sh" ''
              #!/bin/sh

              i3status | while :
              do
                      read line
                      echo "$line"  \
                          | sed 's/WIFI100/󰤨/'  \
                          | sed  's/WIFI[89][[:digit:]]/󰤨/' \
                          | sed  's/WIFI[67][[:digit:]]/󰤥/' \
                          | sed 's/WIFI[345][[:digit:]]/󰤢/' \
                          | sed 's/WIFI[012][[:digit:]]/󰤟/' \
                          | sed 's/BAT9[[:digit:]].*BAT/󰁹/' \
                          | sed 's/BAT8[[:digit:]].*BAT/󰂂/' \
                          | sed 's/BAT7[[:digit:]].*BAT/󰂂/' \
                          | sed 's/BAT6[[:digit:]].*BAT/󰂀/' \
                          | sed 's/BAT5[[:digit:]].*BAT/󰁿/' \
                          | sed 's/BAT4[[:digit:]].*BAT/󰁾/' \
                          | sed 's/BAT3[[:digit:]].*BAT/󰁽/' \
                          | sed 's/BAT2[[:digit:]].*BAT/󰁼/' \
                          | sed 's/BAT1[[:digit:]].*BAT/󰁻/' \
                          | sed 's/BAT0[[:digit:]].*BAT/󰁺/' \
                          | sed 's/MEM\([[:digit:]][[:digit:]]\).*MEM/\1%/' \
                          || exit 1
              done
            '';
          in "${wrapper}";
        }];
        gaps = {
          inner = 10;
          outer = 5;
        };
        window = {
          # hideEdgeBorders = "smart";
          titlebar = false;
        };
        startup = [
          {
            command = "echo hello";
            always = true;
            notification = false;
          }
          {
            command = ''
              "${pkgs.hsetroot}/bin/hsetroot -add \"${colors.base06}\" -add \"${colors.base04}\" -gradient 135"'';
            always = true;
            notification = false;
          }
        ];
      };
    };
  };
}
