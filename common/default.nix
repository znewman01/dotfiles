{ config, pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true;
  nix.settings = {
    substituters = [ "https://nix-community.cachix.org" ];
    trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };
  environment.systemPackages = with pkgs; [ git vim wget ];
}
