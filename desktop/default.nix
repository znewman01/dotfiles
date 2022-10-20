{ pkgs, lib, ... }:

{
  imports = [ ./timezone.nix ./web ./syncthing ];

  homebrew.casks = lib.optionals pkgs.stdenv.isDarwin [ "iterm2" "alfred" ];
}
