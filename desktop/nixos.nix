{ config, pkgs, ... }:

let hosts = [ "zjn@zjn-x1prime" "zjn@zjn-home" "zjn@zjn-work" "zjn-cloud" ];
{
  imports = [
    ./timezone.nix
    ./zfs.nix
    ./system.nix
    ../persist/system.nix
    ./gpg/nixos.nix
  ];

  nix.trustedUsers = [ "@wheel" ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };
  fonts.fontconfig.enable = true;

  users.extraGroups.vboxusers.members = [ "zjn" ];


  programs.dconf.enable = true;
  services.dbus.packages = with pkgs; [ gnome3.dconf ];

  security.pam.loginLimits = [{
    domain = "*";
    type = "soft";
    item = "nofile";
    value = "2048"; # the default of 1024 makes some software (e.g., Rust) choke
  }];


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
