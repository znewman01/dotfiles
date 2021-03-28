{ config, lib, pkgs, ... }:

{
  home.file."bin/beeminder-lichess.sh" = {
    text = "#! /usr/bin/env ${pkgs.nix.out}/bin/nix-shell"
      + builtins.readFile ./beeminder-lichess.sh;
    executable = true;
  };

  systemd.user.services.beeminder-lichess = {
    Unit = { Description = "update beeminder lichess goal"; };

    Service = {
      Type = "oneshot";
      ExecStart = ''
        ${pkgs.bash.out}/bin/bash -c "${config.home.homeDirectory}/${
          config.home.file."bin/beeminder-lichess.sh".target
        }"'';
    };
  };
  systemd.user.timers.beeminder-lichess = {
    Unit = { Description = "update beeminder lichess goal"; };

    Timer = {
      OnCalendar = "hourly";
      RandomizedDelaySec = 3600;
      Unit = "beeminder-lichess.service";
    };

    Install = { WantedBy = [ "timers.target" ]; };
  };
}
