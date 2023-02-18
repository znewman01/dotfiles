{ config, nixpkgs, lib, doom-emacs, impermanence, ... }:

{
  imports = [
    ../../common/nixos.nix
    ../../desktop/nixos.nix
    ./hardware-configuration.nix
    # ../../desktop/work.nix
    # ../../work/default.nix
  ];
  home-manager.users.zjn.imports = [
    ./home.nix
    doom-emacs.hmModule
    impermanence.nixosModules.home-manager.impermanence
  ];
  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = true;

  networking.hostName = "zjn-x1prime";
  networking.hostId = "e2102672";
  networking.networkmanager.enable = true;
  system.stateVersion = "22.11";
}
