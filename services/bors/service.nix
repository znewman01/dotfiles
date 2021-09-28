{ config, pkgs, lib, ... }:

let
  release = pkgs.callPackage ./default.nix { };
  release_name = "bors";
  working_directory = "/run/bors";
  secrets = (import ./secrets.nix); # TODO: long-term solution for this
in {
  users.extraUsers."bors" = {
    isSystemUser = true;
    description = "System user for Bors";
    group = "bors";
  };
  users.groups."bors" = { };
  systemd.tmpfiles.rules = [ "d ${working_directory} 700 bors bors -" ];
  systemd.services.${release_name} = {
    wantedBy = [ "multi-user.target" ];
    after = [ "network.target" "container@postgresBors.service" ];
    requires = [ "network-online.target" "container@postgresBors.service" ];
    description = "Bors";
    environment = {
      RELEASE_TMP = working_directory;
      COMMAND_TRIGGER = "bors";
      # TODO: make all this stuff configurable
      PUBLIC_HOST = "bors.znewman.net";
      DATABASE_URL = "postgresql://postgres@10.100.1.3:5432/bors";
      PORT = "4000";
      DATABASE_USE_SSL = "false";
      ALLOW_PRIVATE_REPOS = "true";
    } // secrets;
    serviceConfig = {
      Type = "exec";
      User = "bors";
      PrivateTmp = true;
      ExecStartPre = ''
        ${release}/bin/${release_name} eval "BorsNG.Database.Migrate.run_standalone"
      '';
      ExecStart = ''
        ${release}/bin/${release_name} start
      '';
      ExecStop = ''
        ${release}/bin/${release_name} stop
      '';
      ExecReload = ''
        ${release}/bin/${release_name} restart
      '';
      Restart = "on-failure";
      RestartSec = 5;
    };
    unitConfig = {
      StartLimitInterval = 10;
      StartLimitBurst = 3;
    };
    # disksup requires bash
    path = [ pkgs.bash ];
  };
}
