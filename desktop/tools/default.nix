{ config, pkgs, ... }:

{
  imports = [ ./direnv.nix ];
  home.packages = with pkgs; [
    tree
    ripgrep
    gh
    sqlite
    imagemagick
    gitsign
    fd
    tree-sitter
    fzf
    anystyle-cli
    gist
  ];
}
