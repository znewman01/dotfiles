{ config, pkgs, ... }:

{
  imports =
    [ ./chat/darwin.nix ./web/darwin-home.nix ./gpg ./home.nix ../emacs ];
}
