{ config, pkgs, lib, inputs, ... }:

{
  imports = [
    ../../common/darwin.nix
    ../../desktop/darwin.nix
  ];

  networking.hostName = "zjn-mac";
  system.stateVersion = 4;

  home-manager.users.zjn = import ./home.nix;
  home-manager.useUserPackages = true;

}
