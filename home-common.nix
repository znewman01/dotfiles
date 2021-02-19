{ config, lib, pkgs, ... }:

with lib;

let dag = config.lib.dag;
in {
  imports = [
    # Modules
    ./modules/code.nix
    ./modules/make-links.nix
    # Config
    ./backup
    ./desktop
    ./emacs
    ./email
    ./web
  ];

  programs.home-manager.enable = true;
  nixpkgs.config.allowUnfree = true;

  xdg.enable = true;

  home.packages = with pkgs; [
    dropbox-cli
    tree
    pass
    ripgrep
    sqlite
    fd
    entr
    libnotify
    pinentry-gtk2
    keybase
    google-chrome
  ];

  services.keybase.enable = true;
  services.kbfs.enable = true;

  programs.ssh = {
    enable = true;
    matchBlocks = {
      "jump.csail.mit.edu" = {
        extraOptions = {
          "GSSAPIAuthentication" = "yes";
          # "GSSAPIKeyExchange" = "yes";
          "VerifyHostKeyDNS" = "yes";
        };
      };
      "*.csail.mit.edu !jump.csail.mit.edu 128.52.* 128.30.* 128.31.*" =
        dag.entryAfter [ "jump.csail.mit.edu" ] {
          extraOptions = {
            "ProxyJump" = "zjn@jump.csail.mit.edu";
            "GSSAPIAuthentication" = "yes";
            "GSSAPIDelegateCredentials" = "yes";
            # "GSSAPIKeyExchange" = "yes";
          };
        };
    };
  };

  programs.direnv = {
    enable = true;
    enableBashIntegration = true;
    enableNixDirenvIntegration = true;
  };

  programs.gpg.enable = true;
  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 86400; # 24 hrs.
    maxCacheTtl = 86400; # 24 hrs.
    pinentryFlavor = "gtk2";
    # enableScDaemon = false;
  };

  programs.git = {
    enable = true;
    ignores = [ "*~" "*.swp" ];
    userEmail = "z@znewman.net";
    userName = "Zachary Newman";
    extraConfig.pull.rebase = "true";
    extraConfig.github.user = "znewman01";
  };

  programs.bash = {
    enable = true;
    sessionVariables = {
      EDITOR = "vim";
      PATH = "$HOME/bin:$PATH";
    };
  };

  home.file."bin/pdftogray" = {
    executable = true;
    text = ''
      #!/usr/bin/env nix-shell
      #!nix-shell -i bash -p ghostscript
      if [ -z "$2" ]; then echo "Usage: $0 <in-file> <out-file>"; exit 1; fi
      gs -sDEVICE=pdfwrite -sProcessColorModel=DeviceGray -sColorConversionStrategy=Gray -dOverrideICC -o $2 -f $1
    '';
  };

  home.links.".password-store" = "Dropbox/passwords";
  home.links.".authinfo.gpg" = "Dropbox/passwords/authinfo.gpg";

  systemd.user.services.dropbox = {
    Unit = { Description = "Dropbox"; };

    Install = { WantedBy = [ "graphical-session.target" ]; };

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

  home.file.".cups/lpoptions".text = ''
    Default 00-dev_null
    Dest xerox8/twoside Duplex=DuplexNoTumble sides=two-sided-long-edge'';

  home.file."bin/beeminder-lichess.sh" = {
    text = ''
      #! /usr/bin/env ${pkgs.nix.out}/bin/nix-shell
      #! nix-shell -i bash -p jq curl pass
      set -euo pipefail
      GOAL=lichess-fast
      BEE="https://www.beeminder.com/api/v1"
      BEE_AUTH="auth_token=$(pass show beeminder-auth-token)"

      TMP_FILE=$(mktemp)
      curl -s "''${BEE}/users/znewman01/goals/lichess-fast/datapoints.json?''${BEE_AUTH}&count=1" > $TMP_FILE
      BEE_GAMES=$(jq first.value $TMP_FILE)
      echo "Current Beeminder # of games: ''${BEE_GAMES}"

      LI_GAMES=$(curl -s https://lichess.org/api/user/znewman01 | jq '.perfs.bullet.games + .perfs.blitz.games')
      echo "Current Lichess # of games: ''${LI_GAMES}"

      if [ $LI_GAMES -eq $BEE_GAMES ]; then
          echo "Beeminder is up-to-date; exiting."
          exit 0
      fi

      echo "Posting the new data point..."
      # TODO: get the response
      DATAPOINT_ID=$(curl -s -X POST \
          --data "''${BEE_AUTH}&value=''${LI_GAMES}" \
          "''${BEE}/users/znewman01/goals/''${GOAL}/datapoints.json" \
          | jq '.id' | sed 's/"//g')

      BAREMIN=$(curl -s "''${BEE}/users/znewman01/goals/lichess-fast?''${BEE_AUTH}&count=1" | jq '.baremin' | sed 's/["+]//g')
      if [ $BAREMIN -ge 0 ]; then
          echo "Didn't derail!"
          exit
      fi

      TOTAL_CHARGE=$(expr 0 - $BAREMIN || true)
      echo "Total to charge: $TOTAL_CHARGE"

      ALREADY_CHARGED=$(jq 'try (first.comment | tonumber) catch 0' $TMP_FILE | sed 's/"//g')
      echo "Already charged today: ''${ALREADY_CHARGED}"

      TO_CHARGE=$(expr $TOTAL_CHARGE - $ALREADY_CHARGED || true)
      echo "Need to charge: ''${TO_CHARGE}"

      if [ $TO_CHARGE -gt 0 ]; then
          echo "Charging..."
          curl -s -X POST "''${BEE}/charges.json?''${BEE_AUTH}&amount=''${TO_CHARGE}&note=lichess-fast"
      else
          echo "Not charging."
      fi
      echo curl -X PUT "''${BEE}/users/znewman01/goals/lichess-fast/datapoints/''${DATAPOINT_ID}.json?''${BEE_AUTH}&comment=$TOTAL_CHARGE"
      curl -X PUT "''${BEE}/users/znewman01/goals/lichess-fast/datapoints/''${DATAPOINT_ID}.json?''${BEE_AUTH}&comment=$TOTAL_CHARGE"

      rm -f "''${TMP_FILE}"
    '';
    executable = true;
  };
  systemd.user.services.beeminder-lichess = {
    Unit = { Description = "update beeminder lichess goal"; };

    Service = {
      Type = "oneshot";
      ExecStart = ''
        ${pkgs.bash.out}/bin/bash -c "${config.home.homeDirectory}/${
          config.home.file."bin/beeminder-lichess.sh".target
        }"'';
    };
  };
  systemd.user.timers.beeminder-lichess = {
    Unit = { Description = "enforce regular restic-based backups"; };

    Timer = {
      OnCalendar = "hourly";
      RandomizedDelaySec = 3600;
      Unit = "beeminder-lichess.service";
    };

    Install = { WantedBy = [ "timers.target" ]; };
  };

  code = {
    baseDir = "${config.home.homeDirectory}/git";
    repos = let
      blackFiles = {
        ".dir-locals.el".text = ''
          ((python-mode . ((blacken-mode . t))))
        '';
      };
    in {
      "fourierhnp" = {
        url = "git@github.com:factorable/fourierhnp.git";
        shell = ./shells/fourier.nix;
        exclude.enable = true;
        extraFiles = blackFiles // {
          ".dir-locals.el".text = ''
            ((python-mode . ((blacken-mode . t)
                             (eval . (require 'vc))
                             (flycheck-pycheckers-max-line-length . 88)
                             (flycheck-pycheckers-pylint-rc .(expand-file-name ".pylintrc" (vc-git-root (buffer-file-name))))
                             (eval . (setenv
                                      "MYPYPATH"
                                      (expand-file-name "python" (vc-git-root (buffer-file-name))))))))
          '';
        };
      };
      "noisy-radio" = {
        url = "git@github.mit.edu:zjn/noisy-radio.git";
        shell = ./shells/noisy-radio.nix;
        exclude.enable = true;
        extraFiles = {
          ".dir-locals.el".text = ''
            ((latex-mode . ((TeX-master . "document.tex"))))
          '';
        };
      };
      "authdict-paper" = {
        url = "git@github.com:alinush/authdict-paper.git";
        shell = ./shells/authdict-paper.nix;
        exclude.enable = true;
      };
      "iacr-dl" = {
        url = "git@github.com:znewman01/iacr-dl.git";
        shell = ./shells/iacr.nix;
        exclude.enable = true;
        extraFiles = blackFiles;
      };
      "resume" = {
        url = "git@github.mit.edu:zjn/resume.git";
        exclude.enable = true;
      };
      "pirate-radio" = {
        url = "git@github.mit.edu:zjn/pirate-radio.git";
        exclude.enable = true;
        shell = ./shells/pirate-radio.nix;
      };
      "sm-proposal" = {
        url = "git@github.mit.edu:zjn/sm-proposal.git";
        exclude.enable = true;
        shell = ./shells/sm-proposal.nix;
        extraFiles = {
          ".dir-locals.el".text = ''
            ((latex-mode . ((TeX-master . "proposal.tex"))))
          '';
        };
      };
      "spectrum-paper" = {
        url = "git@github.com:sachaservan/spectrum-paper.git";
        exclude.enable = true;
        shell = ./shells/spectrum-paper.nix;
      };
      "spectrum-impl" = {
        url = "git@github.com:znewman01/spectrum-impl.git";
        exclude.enable = true;
        shell = ./shells/spectrum.nix;
      };
      "bellman-bignat" = {
        url = "git@github.com:znewman01/bellman-bignat.git";
        exclude.enable = true;
        shell = ./shells/bellman-bignat.nix;
      };
      "vDBx1000" = {
        url = "git@github.com:aluex/vDBx1000.git";
        exclude.enable = true;
      };
      "sm-thesis" = {
        url = "git@github.mit.edu:zjn/sm-thesis.git";
        exclude.enable = true;
        shell = ./shells/sm-thesis.nix;
      };
      "tor-cdn" = {
        url = "git@github.com:iowaguy/tor-cdn.git";
        exclude.enable = true;
        extraFiles = {
          "dataviz/.projectile".text = "";
          "latencies/.projectile".text = "";
        };
        shell = ./shells/tor-cdn.nix;
      };
      "dotfiles" = { url = "git@github.com:znewman01/dotfiles.git"; };
    };
  };
}
