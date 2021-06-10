{ config, pkgs, ... }:

{
  services.xserver.enable = true;
  services.xserver.layout = "us";
  services.xserver.xkbOptions = "caps:swapescape,compose:ralt";
  # https://bugs.launchpad.net/ubuntu/+source/lightdm/+bug/982889
  services.xserver.displayManager.job.preStart = ''
    sleep 3
  '';
  services.xserver.desktopManager.session = [{
    name = "home-manager";
    start = ''
      ${pkgs.runtimeShell} $HOME/.hm-xsession &
      waitPID=$!
    '';
  }];

  # For backlight
  programs.light.enable = true;

  services.redshift = { enable = true; };

  location = {
    latitude = 42.3;
    longitude = -71.1;
  };

  # keep me signed in to skype
  # can't use home-manager due to https://github.com/nix-community/home-manager/issues/1454
  services.gnome.gnome-keyring.enable = true;

  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
  };
  nixpkgs.config.pulseaudio = true;

}
