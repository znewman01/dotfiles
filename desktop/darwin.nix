{ config, pkgs, ... }:

{
  imports = [
    ./timezone.nix
    ./chat/darwin-system.nix
  ];

  homebrew.casks = [ "iterm2" ];
}
