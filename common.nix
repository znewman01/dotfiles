{ config, pkgs, ... }:

let
  home-manager = builtins.fetchGit {
    url = "https://github.com/nix-community/home-manager.git";
    rev = "209566c752c4428c7692c134731971193f06b37c";
    ref = "release-20.09";
  };
in
{
  imports = [ # needs to be absolute since we symlink this file in
    /etc/nixos/hardware-configuration.nix
    (import "${home-manager}/nixos")
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.cleanTmpDir = true;

  boot.kernelPackages = pkgs.linuxPackages_latest;

  networking.networkmanager.enable = true;
  # SBUX wifi
  networking.extraHosts = "172.31.98.1 aruba.odyssys.net";
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

  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
  };
  nixpkgs.config.pulseaudio = true;

  services.openssh.enable = true;
  # TODO: move to home.nix when rycee/home-manager#1087 resolved
  # https://github.com/rycee/home-manager/issues/1087
  programs.ssh.startAgent = true;

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
    extraGroups =
      [ "networkmanager" "wheel" "audio" "video" "lp" "docker" "libvirtd" "systemd-journal" ];
    openssh.authorizedKeys.keyFiles = [ ./net/zjn-x1.pub ];
    hashedPassword = "$6$O1ia1YA5iKh9m$SVD17ySqqyicSpo2tzqTw4xRHm8C50.vMuoQPaLxTA9hsfJ7HQ/neioEYhOjZvPT..HNclbjd4JX4ydBcMvC7.";
  };
  home-manager.verbose = true;
  home-manager.users.zjn = (import ./home.nix) { config = config; pkgs = pkgs; };

  # For backlight
  programs.light.enable = true;

  services.redshift = { enable = true; };

  location = {
    latitude = 42.3;
    longitude = -71.1;
  };

  krb5 = {
    enable = true;
    libdefaults = { forwardable = true; };
  };

  # keep me signed in to skype
  # can't use home-manager due to https://github.com/nix-community/home-manager/issues/1454
  services.gnome3.gnome-keyring.enable = true;
}
