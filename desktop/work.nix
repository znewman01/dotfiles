{ config, pkgs, ... }:

let badhosts = /home/zjn/Sync/documents/badhosts.txt;
in {
  networking.extraHosts =
    if builtins.pathExists badhosts then builtins.readFile badhosts else "";

  services.kolide-launcher = {
    enable = true;
    enrollSecretPath = "/persist/kolide/secret";
    rootDirectory = "/cache/kolide";
    additionalPackages = with pkgs; [ glib zfs networkmanager cryptsetup ];
  };
}
