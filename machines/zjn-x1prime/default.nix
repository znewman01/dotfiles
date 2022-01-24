{ config, pkgs, ... }:

{
  imports = [
    ../../home-common.nix
    ../../code.nix
    ./autorandr.nix
    ./../../email
    ./../../desktop/chat.nix
    ../../services/lichess.nix
  ];
  home.stateVersion = "21.05";
}
