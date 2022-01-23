{ config, pkgs, ... }:

{
  imports = [
    ../../home-common.nix
    ../../code.nix
    ./autorandr.nix
    ./../../email
    ./../../desktop/chat.nix
  ];
  home.stateVersion = "21.05";
}
