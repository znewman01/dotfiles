{ pkgs, ... }:

{
  # imports = [ ];
  home.username = "zjn";
  home.stateVersion = "21.11";
  programs.home-manager.enable = true;

  programs.zsh.enable = true;
}

