{ config, pkgs, lib, ... }:

{
  # I tried briefly to get it running inside a container, but not quite working.
  imports = [ ./bors/service.nix ];

  # Bors needs a Postgres database instance; run one in a systemd container.
  # (We hardcode it in bors/service.nix)
  containers = {
    postgresBors = {
      ephemeral = true;
      privateNetwork = true;
      hostAddress = "10.100.1.2";
      localAddress = "10.100.1.3";
      bindMounts = {
        "/data" = {
          hostPath = "/persist/bors";
          isReadOnly = false;
        };
      };
      config = { config, pkgs, ... }: {
        networking.firewall.allowedTCPPorts = [ 5432 ];
        services.postgresql = {
          enable = true;
          port = 5432;
          enableTCPIP = true;
          dataDir = "/data";
          authentication = "host all all 10.100.0.0/16 trust";
          ensureDatabases = [ "bors" ];
          ensureUsers = [{
            name = "bors";
            ensurePermissions."DATABASE bors" = "ALL PRIVILEGES";
          }];
        };
        system.stateVersion = "21.05";
        systemd.tmpfiles.rules = [ "d /data 700 postgres postgres -" ];
      };
      autoStart = true;
    };
  };

  # Serve bors via ports 443/80 using Nginx with a Let's Encrypt cert.
  networking.firewall.allowedTCPPorts = [ 443 80 ];
  services.nginx = {
    enable = true;
    virtualHosts."bors.znewman.net" = {
      enableACME = true;
      forceSSL = true;
      locations = { "/" = { proxyPass = "http://localhost:4000"; }; };
    };
  };
  security.acme = {
    acceptTerms = true;
    certs."bors.znewman.net".email = "bors@z.znewman.net";
  };
}
