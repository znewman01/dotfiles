{ config, pkgs, ... }:

{
  imports = [ ./home.nix ../persist/home.nix ./i3.nix ./tools ];

  home.packages = with pkgs; [ xclip xdotool pinentry-gtk2 ];

  xdg.enable = true;

  home.keyboard.options = [ "caps:escape" "compose:ralt" ];

  services.udiskie = { enable = true; };
}
