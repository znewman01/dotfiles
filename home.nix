{ config, pkgs, ... }:

{
  programs.home-manager.enable = true;

  programs.git = {
    enable = true;
    ignores = [ "*~" "*.swp" ];
    userEmail = "z@znewman.net";
    userName = "Zachary Newman";
  };
}
