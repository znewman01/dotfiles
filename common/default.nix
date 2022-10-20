{ config, pkgs, doom-emacs, nixpkgs, ... }:

{
  imports = [ ./tailscale ];

  nix.settings = {
    substituters = [ "https://nix-community.cachix.org" ];
    trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };
  nix.registry.nixpkgs.flake = nixpkgs;
  nix.nixPath = [ "nixpkgs=${nixpkgs}" ];

  environment.systemPackages = with pkgs; [ git vim wget ];

  home-manager.extraSpecialArgs = { inherit doom-emacs; };
  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = true;
}
