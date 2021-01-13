{ config, pkgs, ... }:

{
  imports = [ # needs to be absolute since we symlink this file in
    /etc/nixos/hardware-configuration.nix
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.cleanTmpDir = true;

  boot.kernelPackages = pkgs.linuxPackages_latest;

  networking.networkmanager.enable = true;
  # SBUX wifi
  networking.extraHosts = "172.31.98.1 aruba.odyssys.net";

  nixpkgs.config.allowUnfree = true;

  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  i18n.defaultLocale = "en_US.UTF-8";

  time.timeZone = "America/New_York";

  environment.systemPackages = with pkgs; [ git vim wget manpages steam ];

  hardware.opengl.driSupport32Bit = true;

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
  services.xserver.xkbOptions = "caps:swapescape";
  services.xserver.desktopManager.session = [{
    name = "home-manager";
    start = ''
      ${pkgs.runtimeShell} $HOME/.hm-xsession &
      waitPID=$!
    '';
  }];

  services.journald.extraConfig = "MaxRetentionSec=1month";

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
    extraGroups =
      [ "networkmanager" "wheel" "audio" "video" "lp" "docker" "libvirtd" ];
    openssh.authorizedKeys.keyFiles = [ ./net/zjn-x1.pub ];
  };

  system.stateVersion = "19.09";

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
}
