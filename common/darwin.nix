{ config, pkgs, ... }:

{
  imports = [ ./default.nix ];

  programs.zsh.enable = true;

  homebrew = {
    enable = true;
    cleanup = "zap";
  };
  programs.zsh.shellInit = ''eval "$(/opt/homebrew/bin/brew shellenv)"'';

  services.nix-daemon.enable = true;
  nix.useDaemon = true;

  nix.extraOptions = ''
    extra-platforms = aarch64-darwin x86_64-darwin
    experimental-features = nix-command flakes
  '';
  nix.gc = {
    automatic = true;
    interval = { Day = 7; };
    options = "--delete-older-than 30d";
  };
}