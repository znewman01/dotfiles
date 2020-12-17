{ config, lib, pkgs, ... }:

let
  # for Zoom 5.1
  unstableTarball20200727 = builtins.fetchTarball {
  name = "nixos-unstable-2020-07-27";
  url = "https://github.com/nixos/nixpkgs/archive/c83e13315caadef275a5d074243c67503c558b3b.tar.gz";
  # Hash obtained using `nix-prefetch-url --unpack <url>`
  sha256 = "1w8q6y58ddwkxg450i1b98dcalysvx497q3zl7r8py74v4wzrvm5";
};
  # TODO: make module
  fuiTurquoise = "#1abc9c";
  fuiEmerald = "#2ecc71";
  fuiRiver = "#3498db";
  fuiAmethyst = "#9b59b6";
  fuiDeepAsphalt = "#34495e";
  fuiAsphalt = "#425d78";
  fuiSunflower = "#f1c40f";
  fuiCarrot = "#e67e22";
  fuiAlizarin = "#e74c3c";
  fuiClouds = "#ecf0f1";
  fuiConcrete = "#95a5a6";
  fuiDarkTurquoise = "#16a085";
  fuiDarkEmerald = "#27ae60";
  fuiDarkRiver = "#2980b9";
  fuiDarkAmethyst = "#8e44ad";
  fuiDarkAsphalt = "#2c3e50";
  fuiDarkSunflower = "#f39c12";
  fuiDarkCarrot = "#d35400";
  fuiDarkAlizarin = "#c0392b";
  fuiDarkClouds = "#bdc3c7";
  fuiDeepClouds = "#dce0e1";
  fuiDarkConcrete = "#7f8c8d";
  bgColorLight = fuiClouds;
  fgColorLight = fuiAsphalt;
  bgColorDark = fuiDarkAsphalt;
  fgColorDark = fuiDarkClouds;
in
{
  imports = [
    ./xmonad.nix
    ./fonts.nix
    ./alacritty.nix
    ./autorandr.nix
  ];

  nixpkgs.config = {
    packageOverrides = pkgs: {
      unstable20200727 = import unstableTarball20200727 {
        config = config.nixpkgs.config;
      };
    };
  };

  home.packages = with pkgs; [
    anki
    i3lock
    pavucontrol
    xautolock
    xclip
    xdotool
    zathura
    unstable20200727.zoom-us
    slack
    skype
    signal-desktop
    keybase-gui
  ];

  services.screen-locker = {
    enable = true;
    inactiveInterval = 10;
    lockCmd = "${pkgs.i3lock}/bin/i3lock -n";
  };

  xsession = {
    initExtra = ''
      slack &
      signal-desktop &
      keybase-gui &
    '';
    scriptPath = ".hm-xsession";
  };

  home.file."notification.wav".source = ./notification.wav;
  home.file."bin/alert.sh" = {
    text = ''
      #!/usr/bin/env ${pkgs.bash.out}/bin/bash
      ${pkgs.pulseaudio.out}/bin/paplay ${config.home.homeDirectory}/${config.home.file."notification.wav".target}
    '';
    executable = true;
  };
  programs.bash.shellAliases.beep = "notify-send -t 2000 \"done\"";
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
        frame_color = "${fuiDeepAsphalt}";
        frame_width = 3;
      };
      shortcuts = {
        close = "ctrl+space";
        close_all = "ctrl+shift+space";
        history = "ctrl+grave";
        context = "ctrl+shift+comma";
      };
      urgency_low = {
        foreground = "${fuiDarkAsphalt}";
        background = "${bgColorLight}";
      };
      urgency_normal = {
        foreground = "${fgColorLight}";
        background = "${bgColorLight}";
      };
      urgency_critical = {
        frame_color = "${fuiAlizarin}";
        foreground = "${fuiAlizarin}";
        background = "${bgColorLight}";
      };
      play_sound = {
        summary = "*";
        script = "${config.home.homeDirectory}/${config.home.file."bin/alert.sh".target}";
      };
    };
  };
}
