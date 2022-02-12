{ config, pkgs, lib, inputs, ... }:

{
  imports = [
    ../../common.nix
    ../../desktop.nix
    ./hardware-configuration.nix
    ../../desktop/work.nix
  ];
  home-manager.users.zjn.imports = [ ./home.nix inputs.doom-emacs.hmModule ];

  networking.hostName = "zjn-work";
  system.stateVersion = "21.11";
}
