{ config, pkgs, ... }:

{
  imports = [ 
    ./default.nix
    ./tailscale/nixos.nix
    ./ssh/nixos.nix
  ];

  boot.cleanTmpDir = true;

  i18n.defaultLocale = "en_US.UTF-8";

  environment.systemPackages = with pkgs; [ man-pages ];

  virtualisation.docker.enable = true;

  nix.extraOptions = "experimental-features = nix-command flakes";
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 30d";
  };
}
