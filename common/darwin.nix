{ config, pkgs, ... }:

{
  imports = [ ./default.nix ];

  programs.zsh.enable = true;

  users.users.zjn.home = "/Users/zjn";

  homebrew = {
    enable = true;
    onActivation.cleanup = "zap";
    onActivation.upgrade = true;
  };
  programs.zsh.shellInit = ''eval "$(/opt/homebrew/bin/brew shellenv)"'';
}
