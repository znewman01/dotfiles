{ config, pkgs, ... }:

let
  bgColor = "#282A36";
  fgColor = "#F8F8F2";
in
{
  imports = [ ./xmonad.nix ];

  programs.home-manager.enable = true;

  nixpkgs.config.allowUnfree = true;

  xdg.enable = true;
  xsession.enable = true;

  home.packages = with pkgs; [
     dropbox-cli
     haskellPackages.xmobar
     tree
     zathura
     # Fonts
     corefonts
     font-awesome_4
     hack-font
     liberation_ttf
     noto-fonts
     noto-fonts-cjk
     noto-fonts-emoji
     noto-fonts-extra
     ttf_bitstream_vera
  ];


  programs.alacritty = {
    enable = true;
    settings.font.size = 7;
    settings.font.family = [ "Hack" "FontAwesome" ];
    settings.colors = {
      primary = {
        background = "${bgColor}";
        foreground = "${fgColor}";
      };
      normal = {
        black = "0x000000";
        red = "0xff5555";
        green = "0x50fa7b";
        yellow = "0xf1fa8c";
        blue = "0xcaa9fa";
        magenta = "0xff79c6";
        cyan = "0x8be9fd";
        white = "0xbfbfbf";
      };
      bright = {
        black = "0x575b70";
        red = "0xff6e67";
        green = "0x5af78e";
        yellow = "0xf4f99d";
        blue = "0xcaa9fa";
        magenta = "0xff92d0";
        cyan = "0x9aedfe";
        white = "0xe6e6e6";
      };
    };
  };

  programs.git = {
    enable = true;
    ignores = [ "*~" "*.swp" ];
    userEmail = "z@znewman.net";
    userName = "Zachary Newman";
  };

  programs.firefox = {
    enable = true;
    profiles.zjn = {
      name = "Zachary Newman";
      settings = {
        "browser.aboutConfig.showWarning" = false;
        "general.warnOnAboutConfig" = false;
        "browser.startup.homepage" = "about:blank";
        "browser.newtabpage.enabled" = false;
        # For Tridactyl support in more places
        "privacy.resistFingerprinting.block_mozAddonManager" = true;
        "extensions.webextensions.restrictedDomains" = "";
      };
    };
  };
  xdg.configFile."tridactyl/themes/dracula.css".source = ./dracula-tridactyl.css;
  xdg.configFile."tridactyl/tridactylrc".source = ./tridactylrc;


  programs.bash = {
    enable = true;
    sessionVariables = {
      EDITOR = "vim";
    };
  };

  programs.emacs = {
    enable = true;
  };

  home.file.".emacs.d" = {
    recursive = true;
    source = pkgs.fetchFromGitHub {
      owner = "syl20bnr";
      repo = "spacemacs";
      # git ls-remote https://github.com/syl20bnr/spacemacs/ master
      rev = "8c0b8c3440a33df2c291088c47e75fd5e4fbbb61";
      sha256 = "1af73ls0znbpx73ym9cx22yc6q1dvv375l493ccilg2xpjsgam6w";
    };
  };

  fonts.fontconfig.enable = true;

  systemd.user.services.dropbox = {
    Unit = {
      Description = "Dropbox";
    };

    Install = {
      WantedBy = [ "graphical-session.target" ];
    };

    Service = {
      # Hack! See https://github.com/rycee/home-manager/issues/745
      Environment = "QT_PLUGIN_PATH=/run/current-system/sw/${pkgs.qt5.qtbase.qtPluginPrefix}\nEnvironment=QML2_IMPORT_PATH/run/current-system/sw/${pkgs.qt5.qtbase.qtQmlPrefix}";
      ExecStart = "${pkgs.dropbox.out}/bin/dropbox";
      ExecReload = "${pkgs.coreutils.out}/bin/kill -HUP $MAINPID";
      KillMode = "control-group"; # upstream recommends process
      Restart = "on-failure";
      PrivateTmp = true;
      ProtectSystem = "full";
      Nice = 10;
    };
  };
}
