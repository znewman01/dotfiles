{ config, lib, pkgs, ... }:

with lib;

let
  dag = config.lib.dag;
in
{
  imports = [
    # Modules
    ./modules/code.nix
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
  services.gpg-agent.enable = true;

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

  # TODO: factor out into a module (with makeLinks2)
  home.activation.makeLinks = dag.entryAfter [ "writeBoundary" ] ''
      $DRY_RUN_CMD ln -snT \
          "${config.home.homeDirectory}/Dropbox/passwords" \
          "${config.home.homeDirectory}/.password-store" || true
      $DRY_RUN_CMD ln -s \
          "${config.home.homeDirectory}/Dropbox/passwords/authinfo.gpg" \
          "${config.home.homeDirectory}/.authinfo.gpg" || true
  '';

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
    enable = true;
    baseDir = "${config.home.homeDirectory}/git";
    repos = [
      {
        name = "fourierhnp";
        url = "git@github.com:factorable/fourierhnp.git";
        shell = ./shells/fourier.nix;
        exclude.enable = true;
      }
      {
        name = "noisy-radio";
        url = "git@github.mit.edu:zjn/noisy-radio.git";
        shell = ./shells/noisy-radio.nix;
        exclude.enable = true;
      }
    ];
  };
}
