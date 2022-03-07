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
    ./desktop/firefox/nixos.nix
    ./persist/home.nix
    ./common/vim.nix
    ./common/git.nix
    ./desktop/gpg/nixos.nix
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
    imagemagick
  ];
  home.keyboard.options = [ "caps:swapescape" "compose:ralt" ];

  services.keybase.enable = true;
  services.kbfs.enable = true;

  services.syncthing.enable = true;

  programs.ssh.enable = true;

  programs.direnv = {
    enable = true;
    enableBashIntegration = true;
    nix-direnv.enable = true;
  };

  programs.vscode = {
    enable = true;
    package = pkgs.vscodium;
    extensions = with pkgs.vscode-extensions; [
      vscodevim.vim
      golang.go
      eamodio.gitlens
    ];
  };

  programs.bash = {
    enable = true;
    sessionVariables = {
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

}
