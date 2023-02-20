{ config, pkgs, doom-emacs, ... }:

{
  imports = [ ./tailscale ./nix.nix ];

  environment.systemPackages = with pkgs; [ git vim wget ];

  home-manager.extraSpecialArgs = { inherit doom-emacs; };
  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = true;
}
