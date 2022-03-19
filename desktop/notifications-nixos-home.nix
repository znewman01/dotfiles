{ config, pkgs, lib, ... }:

let colors = (import ./colors) { lib = lib; };
in {
  home.file."notification.wav".source = ./notification.wav;
  home.file."bin/alert.sh" = {
    text = ''
      #!/usr/bin/env ${pkgs.bash.out}/bin/bash
      ${pkgs.pulseaudio.out}/bin/paplay ${config.home.homeDirectory}/${
        config.home.file."notification.wav".target
      }
    '';
    executable = true;
  };
  programs.bash.shellAliases.beep = ''notify-send -t 2000 "done"'';

  services.dunst = {
    enable = true;
    settings = {
      global = {
        font = "Iosevka 11";
        format = "<b>%s</b>\\n%b";
        indicate_hidden = true;
        alignment = "center";
        word_wrap = true;
        ignore_newline = false;
        stack_duplicates = true;
        hide_duplicates_count = true;
        geometry = "400x80-30+49"; # [{width}]x{height}[+/-{x}+/-{y}]
        shrink = false;
        idle_threshold = 30;
        follow = "keyboard";
        sticky_history = true;
        line_height = 3;
        separator_height = 3;
        padding = 6;
        horizontal_padding = 6;
        separator_color = "frame";
        frame_color = "#${colors.base05}";
        frame_width = 3;
      };
      shortcuts = {
        close = "ctrl+space";
        close_all = "ctrl+shift+space";
        history = "ctrl+grave";
        context = "ctrl+shift+comma";
      };
      urgency_low = {
        background = "#${colors.base01}";
        foreground = "#${colors.base03}";
      };
      urgency_normal = {
        background = "#${colors.base02}";
        foreground = "#${colors.base05}";
      };
      urgency_critical = {
        background = "#${colors.base08}";
        foreground = "#${colors.base06}";
      };
      play_sound = {
        summary = "*";
        script = "${config.home.homeDirectory}/${
            config.home.file."bin/alert.sh".target
          }";
      };
    };
  };
}
