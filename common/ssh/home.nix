{ config, pkgs, ... }:

{
  home.packages = [ pkgs.openssh ];
  programs.ssh.enable = true;
}
