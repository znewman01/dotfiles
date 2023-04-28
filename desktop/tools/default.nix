{ config, pkgs, ... }:

{
  imports = [ ./direnv.nix ];
  home.packages = with pkgs; [
    tree
    ripgrep
    gh
    sqlite-interactive
    imagemagick
    fd
    # tree-sitter
    fzf
    anystyle-cli
    gist
  ];
}
