{ config, lib, pkgs, ... }:

with lib;

let
  dag = config.lib.dag;
in
{
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
    entr
    libnotify
    pinentry-gtk2
  ];

  programs.direnv = {
    enable = true;
    enableBashIntegration = true;
  };

  programs.gpg.enable = true;
  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 7200;  # 2 hrs.
    pinentryFlavor = "gtk2";
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

  services.lorri.enable = true;

  home.links.".password-store" = "Dropbox/passwords";
  home.links.".authinfo.gpg" = "Dropbox/passwords/authinfo.gpg";

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

  home.file.".cups/lpoptions".text = "Default 00-dev_null\nDest xerox8/twoside Duplex=DuplexNoTumble sides=two-sided-long-edge";

  code = {
    baseDir = "${config.home.homeDirectory}/git";
    repos = let
      blackFiles = {
        ".dir-locals.el".text = "((python-mode . ((blacken-mode . t))))\n";
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
          ".dir-locals.el".text = "((latex-mode . ((TeX-master . \"document.tex\"))))\n";
        };
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
          ".dir-locals.el".text = "((latex-mode . ((TeX-master . \"proposal.tex\"))))\n";
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
    };
  };
}
