{ pkgs, ... }:

{
  imports = [
    ../../home-common.nix
    ../../code.nix
    ./autorandr.nix
    ./../../desktop/email
    ./../../desktop/chat/nixos.nix
  ];
  home.stateVersion = "21.05";
  home.packages = [ pkgs.anki ];
}
