{ config, pkgs, ... }:

{
  imports = [ ./default.nix ];

  i18n.defaultLocale = "en_US.UTF-8";

  environment.systemPackages = with pkgs; [ man-pages ];

  virtualisation.docker.enable = true;
}
