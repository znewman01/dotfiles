{ pkgs, lib, ... }:

{
  imports = [ ./timezone.nix ./web ./syncthing ./chat.nix ];

  homebrew.casks = lib.optionals pkgs.stdenv.isDarwin [
    # "iterm2"
    #  "alfred" 
    "secretive"
  ];
}
