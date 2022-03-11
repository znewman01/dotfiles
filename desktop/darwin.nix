{ config, pkgs, ... }:

{
  imports = [
    ./timezone.nix
    ./chat/darwin-system.nix
    ./web/darwin.nix
    ./syncthing/darwin.nix
    ./emacs
  ];

  homebrew.casks = [ "iterm2" "alfred" ];
}
