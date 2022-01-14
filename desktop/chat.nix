{ config, lib, pkgs, ... }:

let colors = (import ./colors) { lib = lib; };
in {
  home.packages = with pkgs; [
    slack # TODO: change color in $XDG_CONFIG/Slack/storage/root-state.json
    element-desktop
    skype
    signal-desktop
    keybase-gui # TODO: change color in $XDG_CONFIG/keybase/gui_config.json
  ];
  #
  # Slack theme
  # Hack: only way to configure this is to modify the root-state.json file. So we use a proxy file as a trigger.
  xdg.configFile.".slack-color.txt" =
    let slackConfig = "~/.config/Slack/storage/root-state.json";
    in {
      text = colors.mode;
      onChange = ''
        if [ -e ${slackConfig} ]; then
          nix-shell -p jq moreutils --command 'cat ${slackConfig} | jq "setpath([\"settings\", \"userTheme\"]; \"${colors.mode}\")" | sponge ${slackConfig}'
        fi
      '';
    };

  # Keybase theme
  # Same hack as for slack.
  xdg.configFile.".keybase-color.txt" = let
    keybaseConfig = "~/.config/keybase/gui_config.json";
    darkMode = if colors.mode == "dark" then "alwaysDark" else "alwaysLight";
  in {
    text = colors.mode;
    onChange = ''
      if [ -e ${keybaseConfig} ]; then
        nix-shell -p jq moreutils --command 'cat ${keybaseConfig} | jq "setpath([\"ui\", \"darkMode\"]; \"${darkMode}\")" | sponge ${keybaseConfig}'
      fi
    '';
  };

}
