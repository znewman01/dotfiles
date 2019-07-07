{ config, pkgs, ... }:

{
  imports =
    [ # needs to be absolute since we symlink this file in
      /etc/nixos/hardware-configuration.nix
    ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "zjn-x220"; # Define your hostname.
  networking.networkmanager.enable = true;

  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  time.timeZone = "America/New_York";

  environment.systemPackages = with pkgs; [
    git
    vim
    wget
    manpages
  ];

  documentation.dev.enable = true;

  sound.enable = true;

  services.xserver.enable = true;
  services.xserver.layout = "us";
  services.xserver.xkbOptions = "caps:swapescape";

  users.users.zjn = {
    isNormalUser = true;
    extraGroups = [ "networkmanager" "wheel" "audio" "video" ];
  };

  system.stateVersion = "19.09";

  # For backlight
  programs.light.enable = true;

  services.redshift = {
    enable = true;
    latitude = "42.3";
    longitude = "-71.1";
  };
}
