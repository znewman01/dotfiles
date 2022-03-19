{ config, pkgs, ... }:

{
  imports = [
    ./home.nix
    ./ssh/home.nix
  ];
}


