{ config, pkgs, ... }:

{
  programs.home-manager.enable = true;

  xsession.enable = true;
  xsession.windowManager.xmonad.enable = true;

  programs.git = {
    enable = true;
    ignores = [ "*~" "*.swp" ];
    userEmail = "z@znewman.net";
    userName = "Zachary Newman";
  };

  programs.firefox = {
    enable = true;
  };
}
