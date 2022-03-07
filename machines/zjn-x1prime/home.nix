{ pkgs, ... }:

{
  imports = [
    ../../home-common.nix
    ../../code.nix
    ./autorandr.nix
    ./../../desktop/email
    ./../../desktop/chat.nix
  ];
  home.stateVersion = "21.05";
  home.packages = [ pkgs.anki ];
}
