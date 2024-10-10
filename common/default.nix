{ config, pkgs, ... }:

{
  imports = [ ./tailscale ./nix.nix ./ssh ];

  environment.systemPackages = with pkgs; [ git vim wget ];

  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = true;
}
