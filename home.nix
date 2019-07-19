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

  code = {
    baseDir = "${config.home.homeDirectory}/git";
    repos = {
      "fourierhnp" = {
        url = "git@github.com:factorable/fourierhnp.git";
        shell = ./shells/fourier.nix;
        exclude.enable = true;
      };
      "noisy-radio" = {
        url = "git@github.mit.edu:zjn/noisy-radio.git";
        shell = ./shells/noisy-radio.nix;
        exclude.enable = true;
      };
    };
  };
}
