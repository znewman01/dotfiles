{ config, pkgs, ... }:

{
  imports = [
    ../emacs/nixos.nix
    ./home.nix
    ../persist/home.nix
    ../modules/code.nix
    ./xmonad.nix
    ./fonts.nix
    ./screenlock-nixos-home.nix
    ./notifications-nixos-home.nix
    ./theme-nixos-home.nix
    ./tools/nixos.nix
  ];

  home.packages = with pkgs; [
    pavucontrol
    xclip
    xdotool
    pinentry-gtk2
    xcompmgr
  ];

  xsession = {
    profileExtra = ''
      xcompmgr &
      autorandr -c &
    '';
    scriptPath = ".hm-xsession";
  };

  xdg.enable = true;

  home.keyboard.options = [ "caps:swapescape" "compose:ralt" ];

  programs.autorandr.enable = true;
}
