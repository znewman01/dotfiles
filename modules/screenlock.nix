{ config, lib, pkgs, ... }:

with lib;

let cfg = config.screenlock;
in {
  options.screenlock = {
    enable = mkOption {
      type = types.bool;
      description = "Whether to enable screenlock.";
      default = false;
    };
    lockCommand = mkOption {
      type = types.str;
      description = "Command to run to lock the screen.";
      default =
        "${pkgs.i3lock}/bin/i3lock -n -c ${config.colorScheme.colors.base04}";
    };
  };

  config = mkIf cfg.enable {
    services.screen-locker = {
      enable = true;
      inactiveInterval = 10;
      lockCmd = cfg.lockCommand;
    };
  };
}
