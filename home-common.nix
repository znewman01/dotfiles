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
}
