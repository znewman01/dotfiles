{ config, pkgs, ... }:

{
  imports = [ ./direnv.nix ];
  home.packages = with pkgs; [
    tree
    ripgrep
    gh
    sqlite
    imagemagick
    fd
    tree-sitter
    fzf
    anystyle-cli
    gist
  ];
}
