{ config, pkgs, ... }:

{
  imports = [
    ./home.nix
    ./ssh/darwin-home.nix
  ];
}


