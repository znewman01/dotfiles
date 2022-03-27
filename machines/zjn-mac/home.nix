{ pkgs, doom-emacs, ... }:

{
  imports = [
    ../../desktop/darwin-home.nix
    ../../common/home-darwin.nix
    doom-emacs.hmModule
  ];
  home.stateVersion = "21.11";
  programs.home-manager.enable = true;
}
