{ ... }:

{
  imports = [ ../../common/darwin.nix ../../desktop ];
  home-manager.users.zjn.imports = [ ./home.nix ];

  networking.hostName = "zjn-air";
  system.stateVersion = 4;
}
