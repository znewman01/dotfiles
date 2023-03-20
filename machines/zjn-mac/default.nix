{ ... }:

{
  imports = [ ../../common/darwin.nix ../../desktop ];
  home-manager.users.zjn.imports = [ ./home.nix ];

  networking.hostName = "zjn-mac";
  system.stateVersion = 4;
}
