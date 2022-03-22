{ config, pkgs, ... }:

{
  imports = [ ./direnv.nix ];
  home.packages = with pkgs; [
    tree
    ripgrep
    sqlite
    imagemagick
  ];
}
