{ config, pkgs, ... }:

{
  nix.package = pkgs.nixFlakes;

  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; [
    git
    vim
    wget
  ];
}
