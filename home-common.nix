{ config, lib, pkgs, ... }:

with lib;

let
  dag = config.lib.dag;
  tex-env =
    pkgs.texlive.combine { inherit (pkgs.texlive) scheme-full latexmk; };
in {
  imports = [
    # Modules
    ./modules/code.nix
    # Config
    ./desktop
    ./emacs
    ./email
    ./web
    ./services/lichess.nix
    ./persist/home.nix
  ];
  programs.home-manager.enable = true;
  nixpkgs.config.allowUnfree = true;

  xdg.enable = true;

  home.packages = with pkgs; [
    tree
    pass
    ripgrep
    sqlite
    fd
    entr
    libnotify
    keybase
    tex-env
  ];

  services.keybase.enable = true;
  services.kbfs.enable = true;

  services.syncthing.enable = true;

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
      "*.csail.mit.edu !jump.csail.mit.edu 128.52.* 128.30.* 128.31.* !128.31.26.*" =
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
    extraConfig.init.defaultBranch = "main";
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

  systemd.user.tmpfiles.rules = [
    "L %h/.password-store - - - - %h/Sync/passwords"
    "L %h/.authinfo.gpg - - - - %h/Sync/passwords/authinfo.gpg"
  ];

  home.file.".cups/lpoptions".text = ''
    Default 00-dev_null
    Dest xerox8/twoside Duplex=DuplexNoTumble sides=two-sided-long-edge'';

  code = (import ./shells/code.nix) { config = config; };
}
