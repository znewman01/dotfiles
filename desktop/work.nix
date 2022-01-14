{ config, pkgs, ... }:

let badhosts = /home/zjn/Sync/documents/badhosts.txt;
in {
  networking.networkmanager.dns = "dnsmasq";
  services.dnsmasq.enable = true;
  services.dnsmasq.servers = [ "8.8.8.8" "8.8.4.4" ];
  services.dnsmasq.extraConfig = if builtins.pathExists badhosts then
    builtins.readFile badhosts else "";
}
