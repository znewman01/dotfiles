{ config, pkgs, lib, ... }:

let
  darwinOptions = lib.optionalAttrs pkgs.stdenv.isDarwin {
    homebrew.masApps = { Tailscale = 1475387142; };
    homebrew.brews = [ "tailscale" ];
  };
in {
  networking = lib.optionalAttrs pkgs.stdenv.isLinux {
    firewall.trustedInterfaces = [ "tailscale0" ];
    firewall.allowedUDPPorts = [
      41641 # tailscale
    ];
  };
  services.tailscale.enable = pkgs.stdenv.isLinux;
  environment.systemPackages =
    lib.optionals pkgs.stdenv.isLinux [ pkgs.tailscale ];
}
