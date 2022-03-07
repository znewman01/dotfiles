{ config, pkgs, lib, inputs, ... }:

{
  imports = [ ../../common.nix ../../common-darwin.nix ];

  networking.hostName = "zjn-mac";
  system.stateVersion = 4;
}
