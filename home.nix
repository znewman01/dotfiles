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
  ];

  programs.direnv = {
    enable = true;
    enableBashIntegration = true;
  };

  programs.gpg.enable = true;
  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 3600;
  };

  programs.git = {
    enable = true;
    ignores = [ "*~" "*.swp" ];
    userEmail = "z@znewman.net";
    userName = "Zachary Newman";
    extraConfig.pull.rebase = "true";
  };

  programs.bash = {
    enable = true;
    sessionVariables = {
      EDITOR = "vim";
      PATH = "$HOME/bin:$PATH";
    };
  };

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

  home.file."bin/instapaper.py" = {
    source = ./instapaper.py;
    executable = true;
  };

  home.file.".cups/lpoptions".text = "Default 00-dev_null\nDest xerox8/twoside Duplex=DuplexNoTumble sides=two-sided-long-edge";

  code = {
    baseDir = "${config.home.homeDirectory}/git";
    repos = let
      blackFiles = {
        ".dir-locals.el".text = "((python-mode . (blacken-mode . t)))\n";
        ".pycheckers".text = ''
          [DEFAULT]
          max_line_length=88
          pylint_rcfile=.pylintrc
          checkers=pylint,mypy3
        '';
      };
    in {
      "fourierhnp" = {
        url = "git@github.com:factorable/fourierhnp.git";
        shell = ./shells/fourier.nix;
        exclude.enable = true;
        extraFiles = blackFiles;
      };
      "noisy-radio" = {
        url = "git@github.mit.edu:zjn/noisy-radio.git";
        shell = ./shells/noisy-radio.nix;
        exclude.enable = true;
      };
      "iacr-dl" = {
        url = "git@github.com:znewman01/iacr-dl.git";
        shell = ./shells/iacr.nix;
        exclude.enable = true;
        extraFiles = filterAttrs (name: value: name != ".pycheckers") blackFiles;
      };
    };
  };
}
