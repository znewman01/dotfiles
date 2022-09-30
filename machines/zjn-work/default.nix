{ config, nixpkgs, lib, doom-emacs, impermanence, ... }:

{
  imports = [
    ../../common/nixos.nix
    ../../desktop/nixos.nix
    ./hardware-configuration.nix
    ../../desktop/work.nix
    ../../work/default.nix
  ];
  home-manager.users.zjn.imports = [
    ./home.nix
    doom-emacs.hmModule
    impermanence.nixosModules.home-manager.impermanence
  ];

  networking.hostName = "zjn-work";
  system.stateVersion = "21.11";
}
