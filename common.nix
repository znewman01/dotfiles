{ config, pkgs, ... }:

let
  home-manager = builtins.fetchGit {
    url = "https://github.com/nix-community/home-manager.git";
    rev = "148d85ee8303444fb0116943787aa0b1b25f94df";
    ref = "release-21.05";
  };
in {
  imports = [ # needs to be absolute since we symlink this file in
    (import "${home-manager}/nixos")
    ./desktop/system.nix
    ./persist/system.nix
  ];

  nix.trustedUsers = [ "@wheel" ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.cleanTmpDir = true;

  networking.firewall.allowedTCPPorts = [
    3000 # for messing around
    22000 # syncthing
  ];
  networking.firewall.allowedUDPPorts = [
    22000 # syncthing
    21027 # syncthing
    51820 # wireguard
  ];
  networking.firewall.trustedInterfaces = [ "wg0" ];

  networking.extraHosts = ''
    10.100.0.1 zjn-work
    10.100.0.2 zjn-x1prime
    10.100.0.3 zjn-home
  '';

  networking.wireguard.enable = true;
  networking.wireguard.interfaces = {
    wg0 = {
      listenPort = 51820;
      privateKeyFile = "/persist/wireguard/private";
    };
  };

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

  services.openssh = {
    enable = true;
    forwardX11 = true;
  };
  # TODO: move to home.nix when rycee/home-manager#1087 resolved
  # https://github.com/rycee/home-manager/issues/1087
  programs.ssh.startAgent = true;

  services.avahi = {
    enable = true;
    publish = {
      workstation = true;
      enable = true;
      addresses = true;
    };
    nssmdns = true;
  };

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
    openssh.authorizedKeys.keyFiles = (import ./net/keys.nix);
    hashedPassword =
      "$6$O1ia1YA5iKh9m$SVD17ySqqyicSpo2tzqTw4xRHm8C50.vMuoQPaLxTA9hsfJ7HQ/neioEYhOjZvPT..HNclbjd4JX4ydBcMvC7.";
  };
  home-manager.verbose = true;

  krb5 = {
    enable = true;
    libdefaults = {
      forwardable = true;
      default_realm = "CSAIL.MIT.EDU";
    };
  };

  security.pam.loginLimits = [{
    domain = "*";
    type = "soft";
    item = "nofile";
    value = "2048"; # the default of 1024 makes some software (e.g., Rust) choke
  }];

  systemd.services.initdirs = {
    description = "Set up directories if they don't exist.";
    wantedBy = [ "multi-user.target" ];
    after = [ "persist.mount" "cache.mount" ];
    before = [ "home-manager-zjn.service" "sshd.service" ];
    path = [ "/run/current-system/sw/" ];
    script = with pkgs; ''
      set -eux
      mkdir -p /cache/zjn /persist/zjn /persist/ssh
      chown zjn:users /cache/zjn /persist/zjn
    '';
    serviceConfig = { Type = "oneshot"; };
  };

  systemd.services.wgkeys = {
    description = "Set up wireguard keys if they don't exist.";
    wantedBy = [ "multi-user.target" ];
    after = [ "persist.mount" ];
    before = [ "wireguard-wg0.service" ];
    path = [ "/run/current-system/sw/" ];
    script = with pkgs; ''
      set -eux
      mkdir -p /persist/wireguard
      if [ ! -f /persist/wireguard/private ]; then
        umask 077
        wg genkey > /persist/wireguard/private
        wg pubkey < /persist/wireguard/private > /persist/wireguard/public
      fi
    '';
    serviceConfig = { Type = "oneshot"; };
  };
}
