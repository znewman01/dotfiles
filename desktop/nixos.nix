{ config, pkgs, ... }:

{
  imports = [ ./zfs.nix ./system.nix ../persist/system.nix ./timezone.nix ];

  nix.settings.trusted-users = [ "@wheel" ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelPackages = config.boot.zfs.package.latestCompatibleLinuxPackages;

  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };
  fonts.fontconfig.enable = true;

  programs.dconf.enable = true;
  services.dbus.packages = with pkgs; [ dconf ];

  services.udisks2 = {
    enable = true;
    mountOnMedia = true;
  };

  users.users.zjn = {
    isNormalUser = true;
    createHome = true;
    group = "zjn";
    extraGroups = [
      "networkmanager"
      "wheel"
      "audio"
      "video"
      "lp"
      "docker"
      "libvirtd"
      "systemd-journal"
    ];
    hashedPassword =
      "$6$O1ia1YA5iKh9m$SVD17ySqqyicSpo2tzqTw4xRHm8C50.vMuoQPaLxTA9hsfJ7HQ/neioEYhOjZvPT..HNclbjd4JX4ydBcMvC7.";
  };
  users.groups.zjn = { };
}
