{ config, pkgs, ... }:

{
  programs.home-manager.enable = true;

  xsession.enable = true;
  xsession.windowManager.xmonad = {
    enable = true;
    config = pkgs.writeText "xmonad.hs" ''
      import XMonad

      main = xmonad defaultConfig
          { terminal = "st" }
    '';
  };

  home.packages = [ pkgs.st ];

  programs.git = {
    enable = true;
    ignores = [ "*~" "*.swp" ];
    userEmail = "z@znewman.net";
    userName = "Zachary Newman";
  };

  programs.firefox = {
    enable = true;
  };


  programs.bash = {
    enable = true;
    sessionVariables = {
      EDITOR = "vim";
    };
  };
}
