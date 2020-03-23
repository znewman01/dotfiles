{ config, lib, pkgs, ... }:

{
  imports = [
    ./xmonad.nix
    ./fonts.nix
    ./alacritty.nix
    ./autorandr.nix
  ];

  home.packages = with pkgs; [
    anki
    i3lock
    xautolock
    xclip
    xdotool
    zathura
    slack
  ];

  services.screen-locker = {
    enable = true;
    inactiveInterval = 10;
    lockCmd = "${pkgs.i3lock}/bin/i3lock -n";
  };

  services.dunst = {
    enable = true;
    settings = {
      global = {
        font = "Hack 11";
        format = "<b>%s</b>\\n%b";
        indicate_hidden = true;
        alignment = "center";
        word_wrap = true;
        ignore_newline = false;
        stack_duplicates = true;
        hide_duplicates_count = true;
        geometry = "400x80-30+49"; #   [{width}]x{height}[+/-{x}+/-{y}]
        shrink = false;
        idle_threshold = 30;
        follow = "keyboard";
        sticky_history = true;
        line_height = 3;
        separator_height = 3;
        padding = 6;
        horizontal_padding = 6;
        separator_color = "frame";
        frame_color = "#44475A";
        frame_width = 3;
      };
      shortcuts = {
        close = "ctrl+space";
        close_all = "ctrl+shift+space";
        history = "ctrl+grave";
        context = "ctrl+shift+period";
      };
      urgency_low = {
        foreground = "#6272A4";
        background = "#282A36";
        timeout = 4;
      };
      urgency_normal = {
        foreground = "#f8f8f2";
        background = "#282A36";
        timeout = 6;
      };
      urgency_critical = {
        frame_color = "#FF5555";
        foreground = "#FF5555";
        background = "#282A36";
        timeout = 8;
      };
    };
  };
}
