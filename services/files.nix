{ config, pkgs, lib, ... }:

{
  networking.firewall.allowedTCPPorts = [ 443 80 ];
  services.nginx = {
    enable = true;
    virtualHosts."files.znewman.net" = {
      enableACME = true;
      forceSSL = true;
      locations = { "/" = { root = "/var/lib/syncthing/default/public/"; }; };
    };
  };
  systemd.tmpfiles.rules = [ "z /var/lib/syncthing 755 syncthing syncthing -" ];

}
