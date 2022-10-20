{ config, pkgs, ... }:

{
  imports = [ ./chat/darwin.nix ./home.nix ./tools ../emacs ];
}
