{ config, pkgs, lib, ... }:

{
  networking = lib.optionalAttrs pkgs.stdenv.isLinux {
    firewall.trustedInterfaces = [ "tailscale0" ];
    firewall.allowedUDPPorts = [
      41641 # tailscale
    ];
  };
  services.tailscale.enable = pkgs.stdenv.isLinux;
  environment.systemPackages =
    lib.optionals pkgs.stdenv.isLinux [ pkgs.tailscale ];

  homebrew = lib.optionalAttrs pkgs.stdenv.isDarwin {
    masApps = { Tailscale = 1475387142; };
    brews = [ "tailscale" ];
  };
}

