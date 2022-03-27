{ config, pkgs, ... }:

{
  imports = [ ./home.nix ./ssh/darwin-home.nix ];

  home.username = "zjn";

  programs.zsh.enable = true;
  programs.git.ignores = [ ".DS_Store" ];
}
