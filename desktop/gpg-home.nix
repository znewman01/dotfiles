{ config, pkgs, lib, ... }:

{
  programs.gpg.enable = true;

  services.gpg-agent.enable = pkgs.stdenv.isLinux;
  services.gpg-agent = {
    defaultCacheTtl = 86400; # 24 hrs.
    maxCacheTtl = 86400; # 24 hrs.
    pinentryFlavor = "gtk2";
  };
}
