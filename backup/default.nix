{ config, lib, pkgs, ... }:

let
  backup-scripts = import ./backup-scripts.nix;
in
{
  home.packages = [
    backup-scripts
  ];

  systemd.user.services.backups = {
    Unit = {
      Description = "regular restic-based backups";
    };

    Service = {
      Type = "oneshot";
      ExecStart = "${backup-scripts}/bin/backup.sh";
    };
  };

  systemd.user.timers.backups = {
    Unit = {
      Description = "regular restic-based backups";
    };

    Timer = {
      OnCalendar = "hourly";
      RandomizedDelaySec = 3600;
      Unit = "backups.service";
    };

    Install = {
      WantedBy = [ "timers.target" ];
    };
  };

  systemd.user.services.ensure-backups-up-to-date = {
    Unit = {
      Description = "enforce regular restic-based backups";
    };

    Service = {
      Type = "oneshot";
      ExecStart = "${backup-scripts}/bin/enforce_backup_recency.sh";
    };
  };

  systemd.user.timers.ensure-backups-up-to-date = {
    Unit = {
      Description = "enforce regular restic-based backups";
    };

    Timer = {
      OnCalendar = "hourly";
      RandomizedDelaySec = 3600;
      Unit = "ensure-backups-up-to-date.service";
    };

    Install = {
      WantedBy = [ "timers.target" ];
    };
  };

  systemd.user.services.prune-backups = {
    Unit = {
      Description = "prune regular restic-based backups";
    };

    Service = {
      Type = "oneshot";
      ExecStart = "${backup-scripts}/bin/prune.sh";
    };
  };

  systemd.user.timers.prune-backups = {
    Unit = {
      Description = "prune regular restic-based backups";
    };

    Timer = {
      OnCalendar = "hourly";
      RandomizedDelaySec = 3600;
      Unit = "prune-backups.service";
    };

    Install = {
      WantedBy = [ "timers.target" ];
    };
  };
}
