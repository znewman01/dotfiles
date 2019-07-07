{ config, lib, pkgs, ... }:

with lib;

let
  bgColor = "#282A36";
  fgColor = "#F8F8F2";
  dag = config.lib.dag;
  passBin = "${pkgs.pass}/bin/pass";
in
{
  imports = [ ./xmonad.nix ./code.nix ];

  programs.home-manager.enable = true;

  nixpkgs.config.allowUnfree = true;

  xdg.enable = true;
  xsession.enable = true;

  home.packages = with pkgs; [
     dropbox-cli
     haskellPackages.xmobar
     tree
     zathura
     mu
     pass
     xclip
     xautolock
     i3lock
     anki
     # Fonts
     corefonts
     source-code-pro
     font-awesome_4
     hack-font
     liberation_ttf
     noto-fonts
     noto-fonts-cjk
     noto-fonts-emoji
     noto-fonts-extra
     ttf_bitstream_vera
  ];

  programs.direnv = {
    enable = true;
    enableBashIntegration = true;
  };

  programs.gpg.enable = true;
  services.gpg-agent.enable = true;

  # concatStringsSep trick is a half-hearted attempt to prevent email harvesting.
  accounts.email = {
    maildirBasePath = "${config.home.homeDirectory}/Maildir";

      accounts = {
      mit = {
        address = concatStringsSep "@" [ "zjn" "mit.edu" ];
        userName = concatStringsSep "@" [ "zjn" "mit.edu" ];
        passwordCommand = "${passBin} show mit";
        folders = {
          inbox = "Inbox";
          drafts = "Drafts";
          sent = "Sent";
          trash = "Deleted";
        };
        imap = {
          host = "imap.exchange.mit.edu";
          port = 993;
          tls.enable = true;
        };
        mbsync = {
          enable = true;
          create = "both";
          expunge = "both";
          remove = "maildir";
          flatten = "..";
          patterns = [ "Archive" "Drafts" "INBOX" ];
          extraConfig.account = {
            AuthMechs = "PLAIN";
          };
        };
      };

      fastmail = {
        realName = "Zachary Newman";
        userName = concatStringsSep "@" [ "z" "znewman.net" ];
        address = concatStringsSep "@" [ "z" "znewman.net" ];
        maildir.path = "fastmail";
        passwordCommand = "${passBin} show fastmail";
        primary = true;
        folders = {
          inbox = "INBOX";
          drafts = "Drafts";
          sent = "Sent";
          trash = "Trash";
        };
        imap = {
          host = "imap.fastmail.com";
          port = 993;
          tls.enable = true;
        };
        mbsync = {
          enable = true;
          create = "both";
          expunge = "both";
          remove = "maildir";
          flatten = "..";
          patterns = [ "Archive" "Drafts" "INBOX" "Sent" "Spam" "Trash" ];
        };
      };

      gmail = {
        realName = "Zachary Newman";
        address = concatStringsSep "@" [ "znewman01" "gmail.com" ];
        userName = concatStringsSep "@" [ "znewman01" "gmail.com" ];
        maildir.path = "gmail";
        flavor = "gmail.com";
        passwordCommand = "${passBin} show gmail-imap";
        folders = {
          inbox = "Inbox";
          drafts = "[Gmail]/Drafts";
          sent = "[Gmail]/SentMail";
          trash = "[Gmail]/Trash";
        };
        imap = {
          port = 993;
        };
        mbsync = {
          enable = true;
          create = "both";
          expunge = "both";
          remove = "maildir";
          patterns = [ "Inbox" "\"[Gmail]/Drafts\"" "\"[Gmail]/Spam\"" "\"[Gmail]/Trash\"" ];
          extraConfig.account = {
            AuthMechs = "LOGIN";
          };
          extraConfig.channel = {
            CopyArrivalDate = "yes";
          };
        };
      };
    };
  };
  programs.mbsync = {
    enable = true;
    extraConfig = ''
      # Blocked on https://github.com/rycee/home-manager/issues/748
      # CopyArrivalDate yes

      Channel mit-sent
      Create Both
      Expunge Both
      Master ":mit-remote:Sent Items"
      Slave ":mit-local:Sent"

      Channel mit-junk
      Create Both
      Expunge Both
      Master ":mit-remote:Junk E-Mail"
      Slave ":mit-local:Junk"

      Channel mit-trash
      Create Both
      Expunge Both
      Master ":mit-remote:Deleted Items"
      Slave ":mit-local:Deleted"

      Group mit
      Channel mit
      Channel mit-sent
      Channel mit-junk
      Channel mit-junk

      Channel gmail-inbox
      CopyArrivalDate yes
      Create Both
      Expunge Both
      Master ":gmail-remote:Inbox"
      Slave ":gmail-local:Inbox"

      Channel gmail-sent
      CopyArrivalDate yes
      Create Both
      Expunge Both
      Master ":gmail-remote:[Gmail]/Sent Mail"
      Slave ":gmail-local:[Gmail]/SentMail"

      Channel gmail-all
      CopyArrivalDate yes
      Create Both
      Expunge Both
      Master ":gmail-remote:[Gmail]/All Mail"
      Slave ":gmail-local:[Gmail]/AllMail"

      Group gmail
      Channel gmail
      Channel gmail-inbox
      Channel gmail-sent
      Channel gmail-all
    '';
  };
  services.mbsync = {
    enable = true;
    postExec = ("${pkgs.emacs}/bin/emacsclient -e" +
                " \"(progn (require 'mu4e) (mu4e-update-index))\"");
  };


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
      PATH = "$HOME/bin:$PATH";
    };
  };

  home.file."bin/em" = {
    text = "#!/bin/sh\nemacsclient -nc $@";
    executable = true;
  };

  programs.emacs = {
    enable = true;
  };

  services.emacs.enable = true;

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
  home.activation.makeLinks = dag.entryAfter [ "writeBoundary" ] ''
      $DRY_RUN_CMD ln -snT \
          "${config.home.homeDirectory}/Dropbox/dotfiles/spacemacs.d" \
          "${config.home.homeDirectory}/.spacemacs.d" || true
      $DRY_RUN_CMD ln -snT \
          "${config.home.homeDirectory}/Dropbox/notes" \
          "${config.home.homeDirectory}/notes" || true
      $DRY_RUN_CMD ln -snT \
          "${config.home.homeDirectory}/Dropbox/passwords" \
          "${config.home.homeDirectory}/.password-store" || true
      $DRY_RUN_CMD ln -s \
          "${config.home.homeDirectory}/Dropbox/passwords/authinfo.gpg" \
          "${config.home.homeDirectory}/.authinfo.gpg" || true
  '';

  fonts.fontconfig.enable = true;

  systemd.user.services.dropbox = {
    Unit = {
      Description = "Dropbox";
    };

    Install = {
      WantedBy = [ "graphical-session.target" ];
    };

    Service = {
      Environment = [
        "QT_PLUGIN_PATH=/run/current-system/sw/${pkgs.qt5.qtbase.qtPluginPrefix}"
        "QML2_IMPORT_PATH=/run/current-system/sw/${pkgs.qt5.qtbase.qtQmlPrefix}"
      ];
      ExecStart = "${pkgs.dropbox.out}/bin/dropbox";
      ExecReload = "${pkgs.coreutils.out}/bin/kill -HUP $MAINPID";
      KillMode = "control-group"; # upstream recommends process
      Restart = "on-failure";
      PrivateTmp = true;
      ProtectSystem = "full";
      Nice = 10;
    };
  };

  home.file."bin/instapaper.py" = {
    source = ./instapaper.py;
    executable = true;
  };
}
