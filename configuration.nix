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
  ];

  # Enable sound.
  # sound.enable = true;
  # hardware.pulseaudio.enable = true;

  services.xserver.enable = true;
  services.xserver.layout = "us";
  services.xserver.xkbOptions = "caps:swapescape";

  users.users.zjn = {
    isNormalUser = true;
    extraGroups = [ "networkmanager" "wheel" ];
  };

  system.stateVersion = "19.03";
}
