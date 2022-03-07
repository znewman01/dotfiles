{ config, pkgs, ... }:

{
  imports = [
    ./timezone.nix
    ./chat/darwin-system.nix
    ./web/darwin.nix
  ];

  homebrew.casks = [ "iterm2" ];
}
