{ config, pkgs, ... }:

{
  imports = [
    # Modules
    ./modules/code.nix
    # Config
    ./desktop
    ./emacs
    ./desktop/firefox/nixos.nix
    ./persist/home.nix
    ./desktop/home-common.nix
  ];

  xdg.enable = true;

  home.keyboard.options = [ "caps:swapescape" "compose:ralt" ];

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
