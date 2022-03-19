{ config, pkgs, ... }:

{
  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 86400; # 24 hrs.
    maxCacheTtl = 86400; # 24 hrs.
    pinentryFlavor = "gtk2";
    # enableScDaemon = false;
  };
}
