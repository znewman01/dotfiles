{ config, pkgs, lib, inputs, ... }:

{
  imports = [
    ../../common.nix
    ../../desktop.nix
    ./hardware-configuration.nix
    ./../../desktop/work.nix
    ./../../desktop/rotate-webcam.nix
  ];
  home-manager.users.zjn.imports = [ ./home.nix inputs.doom-emacs.hmModule ];

  boot.kernelPackages = pkgs.linuxPackages_5_10;

  networking.hostName = "zjn-home";
  system.stateVersion = "20.09";
}
