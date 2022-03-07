{ pkgs, ... }:

{
  imports = [
    ../../desktop/darwin-home.nix
    ../../common/home-darwin.nix
  ];
  home.username = "zjn";
  home.stateVersion = "21.11";

  programs.home-manager.enable = true;
  programs.zsh.enable = true;
}

