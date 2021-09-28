{ config, pkgs, lib, ... }:

{
  networking.firewall.allowedTCPPorts = [ 443 80 ];
  services.nginx = {
    enable = true;
    virtualHosts."files.znewman.net" = {
      enableACME = true;
      forceSSL = true;
      locations = { "/" = { root = "/persist/zjn/Sync/public/"; }; };
    };
  };
  security.acme = {
    acceptTerms = true;
    certs."files.znewman.net".email = "letsencrypt@z.znewman.net";
  };
}
