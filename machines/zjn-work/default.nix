{ config, nixpkgs, lib, doom-emacs, ... }:

{
  imports = [
    ../../common/nixos.nix
    ../../desktop/nixos.nix
    ./hardware-configuration.nix
    ../../desktop/work.nix
  ];
  home-manager.users.zjn.imports = [ ./home.nix doom-emacs.hmModule ];

  networking.hostName = "zjn-work";
  system.stateVersion = "21.11";
}
