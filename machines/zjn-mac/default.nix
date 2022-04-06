{ config, pkgs, lib, doom-emacs, ... }:

{
  imports = [ ../../common/darwin.nix ../../desktop/darwin.nix ];

  networking.hostName = "zjn-mac";
  system.stateVersion = 4;

  users.users.zjn.home = "/Users/zjn";
  home-manager.users.zjn = import ./home.nix;
  home-manager.extraSpecialArgs = { inherit doom-emacs; };
  home-manager.useUserPackages = true;
}
