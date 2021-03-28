{ config, pkgs, ... }:

let
  home-manager = builtins.fetchGit {
    url = "https://github.com/nix-community/home-manager.git";
    rev = "209566c752c4428c7692c134731971193f06b37c";
    ref = "release-20.09";
  };
in {
  imports = [ # needs to be absolute since we symlink this file in
    /etc/nixos/hardware-configuration.nix
    (import "${home-manager}/nixos")
    ./desktop/system.nix
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.cleanTmpDir = true;

  boot.kernelPackages = pkgs.linuxPackages_latest;

  networking.networkmanager.enable = true;
  networking.firewall.allowedTCPPorts = [
    3000 # for messing around
    22000 # syncthing
  ];
  networking.firewall.allowedUDPPorts = [
    22000 # syncthing
    21027 # syncthing
  ];

  nixpkgs.config.allowUnfree = true;

  services.dbus.packages = with pkgs; [ gnome3.dconf ];

  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };
  fonts.fontconfig.enable = true;

  i18n.defaultLocale = "en_US.UTF-8";

  time.timeZone = "America/New_York";

  environment.systemPackages = with pkgs; [ git vim wget manpages ];

  documentation.dev.enable = true;

  services.openssh.enable = true;
  # TODO: move to home.nix when rycee/home-manager#1087 resolved
  # https://github.com/rycee/home-manager/issues/1087
  programs.ssh.startAgent = true;

  services.printing.enable = true;
  services.printing.clientConf = ''
    ServerName cups.csail.mit.edu
  '';
  services.printing.extraConf = ''
    SystemGroup lp
  '';

  virtualisation.docker.enable = true;
  virtualisation.libvirtd.enable = true;
  users.extraGroups.vboxusers.members = [ "zjn" ];

  users.users.zjn = {
    isNormalUser = true;
    createHome = true;
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
    openssh.authorizedKeys.keyFiles = [ ./net/zjn-x1.pub ./net/zjn-home.pub ];
    hashedPassword =
      "$6$O1ia1YA5iKh9m$SVD17ySqqyicSpo2tzqTw4xRHm8C50.vMuoQPaLxTA9hsfJ7HQ/neioEYhOjZvPT..HNclbjd4JX4ydBcMvC7.";
  };
  home-manager.verbose = true;
  home-manager.users.zjn = (import ./home.nix) {
    config = config;
    pkgs = pkgs;
  };

  krb5 = {
    enable = true;
    libdefaults = { forwardable = true; };
  };
}
