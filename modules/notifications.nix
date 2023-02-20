{ config, lib, pkgs, ... }:

with lib;

let cfg = config.notifications;
in {
  options.notifications = {
    enable = mkOption {
      type = types.bool;
      description = "Whether to enable notifications.";
      default = false;
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ pkgs.libnotify ];
    services.dunst = {
      enable = true;
      settings = let colors = config.colorScheme.colors;
      in {
        global = {
          frame_color = "#${colors.base05}";
          separator_color = "#${colors.base05}";
          font = let font = builtins.head config.fonts.terminalFonts;
          in "${font.name} ${builtins.toString (builtins.floor font.size)}";
        };
        urgency_low = {
          background = "#${colors.base02}";
          foreground = "#${colors.base05}";
        };
        urgency_normal = {
          background = "#${colors.base01}";
          foreground = "#${colors.base03}";
        };
        urgency_critical = {
          background = "#${colors.base08}";
          foreground = "#${colors.base06}";
        };
      };
    };
    programs.bash.shellAliases = { beep = "notify-send 'done'"; };
  };
}
