{ config, pkgs, ... }:

{
  imports = [ ./home.nix ./ssh/darwin-home.nix ];

  home.username = "zjn";
}


