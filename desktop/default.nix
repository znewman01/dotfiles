{ config, lib, pkgs, ... }:

with lib;

let
  # for Zoom 5.1
  unstableTarball20210207 = builtins.fetchTarball {
    name = "nixos-unstable-2021-02-07";
    url =
      "https://github.com/nixos/nixpkgs/archive/a21a3bd84885c20f7736be84698595cc572c1454.tar.gz";

    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "0sbfv0dw5l13hf8pl0im0xrybnl5vnq3x0sv2x7v6i7hh46ch34a";

  };
  colors = (import ./colors) { lib = lib; };
in {
  imports = [ ./xmonad.nix ./fonts.nix ./alacritty.nix ];

  nixpkgs.config = {
    packageOverrides = pkgs: {
      unstable20210207 =
        import unstableTarball20210207 { config = config.nixpkgs.config; };
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
    unstable20210207.zoom-us
    slack # TODO: change color in $XDG_CONFIG/Slack/storage/root-state.json
    skype
    signal-desktop
    keybase-gui # TODO: change color in $XDG_CONFIG/keybase/gui_config.json
    gist
    xcompmgr
  ];

  # Slack theme
  # Hack: only way to configure this is to modify the root-state.json file. So we use a proxy file as a trigger.
  xdg.configFile."Slack/storage/color.txt" =
    let slackConfig = "~/.config/Slack/storage/root-state.json";
    in {
      text = colors.mode;
      onChange = ''
        nix-shell -p jq moreutils --command 'cat ${slackConfig} | jq "setpath([\"settings\", \"userTheme\"]; \"${colors.mode}\")" | sponge ${slackConfig}'
      '';
    };

  # Keybase theme
  # Same hack as for slack.
  xdg.configFile."keybase/color.txt" = let
    keybaseConfig = "~/.config/keybase/gui_config.json";
    darkMode = if colors.mode == "dark" then "alwaysDark" else "alwaysLight";
  in {
    text = colors.mode;
    onChange = ''
      nix-shell -p jq moreutils --command 'cat ${keybaseConfig} | jq "setpath([\"ui\", \"darkMode\"]; \"${darkMode}\")" | sponge ${keybaseConfig}'
    '';
  };

  programs.autorandr.enable = true;

  services.screen-locker = {
    enable = true;
    inactiveInterval = 10;
    lockCmd = "${pkgs.i3lock}/bin/i3lock -n -c ${colors.base00}";
  };

  xsession = {
    profileExtra = ''
      xcompmgr &
      autorandr -c &
    '';
    scriptPath = ".hm-xsession";
  };

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
  gtk = {
    enable = true;
    theme = {
      package = pkgs.arc-theme;
      name = if colors.mode == "dark" then "Arc-Dark" else "Arc";
    };
    font = { name = "Bitstream Vera Sans 12"; };
  };
  home.file.".Xresources".text = ''
    ! Base16 {{scheme-name}}
    ! Scheme: {{scheme-author}}

    #define base00 #${colors.base00}
    #define base01 #${colors.base01}
    #define base02 #${colors.base02}
    #define base03 #${colors.base03}
    #define base04 #${colors.base04}
    #define base05 #${colors.base05}
    #define base06 #${colors.base06}
    #define base07 #${colors.base07}
    #define base08 #${colors.base08}
    #define base09 #${colors.base09}
    #define base0A #${colors.base0A}
    #define base0B #${colors.base0B}
    #define base0C #${colors.base0C}
    #define base0D #${colors.base0D}
    #define base0E #${colors.base0E}
    #define base0F #${colors.base0F}

    *foreground:   base05
    #ifdef background_opacity
    *background:   [background_opacity]base00
    #else
    *background:   base00
    #endif
    *cursorColor:  base05

    *color0:       base00
    *color1:       base08
    *color2:       base0B
    *color3:       base0A
    *color4:       base0D
    *color5:       base0E
    *color6:       base0C
    *color7:       base05

    *color8:       base03
    *color9:       base08
    *color10:      base0B
    *color11:      base0A
    *color12:      base0D
    *color13:      base0E
    *color14:      base0C
    *color15:      base07

    ! Note: colors beyond 15 might not be loaded (e.g., xterm, urxvt),
    ! use 'shell' template to set these if necessary
    *color16:      base09
    *color17:      base0F
    *color18:      base01
    *color19:      base02
    *color20:      base04
    *color21:      base06
  '';
}
