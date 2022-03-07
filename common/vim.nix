{ config, pkgs, ... }:

{
  home.packages = [ pkgs.vim ];
  home.sessionVariables.EDITOR = "vim"; 
}


