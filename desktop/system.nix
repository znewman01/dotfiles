{ config, pkgs, ... }:

{
  services.xserver.enable = true;
  services.xserver.layout = "us";
  services.xserver = {
    # https://bugs.launchpad.net/ubuntu/+source/lightdm/+bug/982889
    displayManager.job.preStart = ''
      sleep 3
    '';
    desktopManager.session = [
      {
        name = "xsession";
        start = ''
          ${pkgs.runtimeShell} $HOME/.xsession &
          waitPID=$!
        '';
      }
    ];
  };

  # For backlight
  programs.light.enable = true;

  services.redshift = { enable = true; };

  location = {
    latitude = 42.3;
    longitude = -71.1;
  };

  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
  };
  nixpkgs.config.pulseaudio = true;
}
