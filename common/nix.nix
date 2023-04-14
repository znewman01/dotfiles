{ config, lib, pkgs, nixpkgs, ... }:

{
  nix = {
    settings = {
      substituters = [ "https://nix-community.cachix.org" ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
    };
    registry.nixpkgs.flake = nixpkgs;
    nixPath = [ "nixpkgs=${nixpkgs}" ];
    extraOptions = "experimental-features = nix-command flakes";
    gc = {
      automatic = true;
      options = "--delete-older-than 30d";
    } // (lib.optionalAttrs pkgs.stdenv.isDarwin {
      interval = { Day = 7; };
      user = "zjn";
    }) // (lib.optionalAttrs pkgs.stdenv.isLinux { dates = "weekly"; });
  } // (lib.optionalAttrs pkgs.stdenv.isDarwin { useDaemon = true; });
  services.nix-daemon.enable = true;
}
