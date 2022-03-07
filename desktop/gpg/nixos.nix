{ config, pkgs, ... }:

{
  imports = [ ./default.nix ];

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 86400; # 24 hrs.
    maxCacheTtl = 86400; # 24 hrs.
    pinentryFlavor = "gtk2";
    # enableScDaemon = false;
  };
}
