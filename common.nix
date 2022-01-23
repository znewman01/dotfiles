{ config, pkgs, ... }:

let
  home-manager = builtins.fetchGit {
    url = "https://github.com/nix-community/home-manager.git";
    rev = "697cc8c68ed6a606296efbbe9614c32537078756";
    ref = "release-21.11";
  };
  hosts = [ "zjn-x1prime" "zjn-home" ];
in {
  imports = [ # needs to be absolute since we symlink this file in
    (import "${home-manager}/nixos")
    ./desktop/system.nix
    ./persist/system.nix
  ];

  nix.trustedUsers = [ "@wheel" ];
  nix.package = pkgs.nixFlakes;
  nix.extraOptions = "experimental-features = nix-command flakes";

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
    41641 # tailscale
  ];
  services.tailscale.enable = true;

  nixpkgs.config.allowUnfree = true;

  programs.dconf.enable = true;
  services.dbus.packages = with pkgs; [ gnome3.dconf ];

  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };
  fonts.fontconfig.enable = true;

  i18n.defaultLocale = "en_US.UTF-8";

  time.timeZone = "America/New_York";

  environment.systemPackages = with pkgs; [ git vim wget manpages tailscale ];

  documentation.dev.enable = true;

  services.openssh = {
    enable = true;
    forwardX11 = true;
    passwordAuthentication = false;
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

  virtualisation.docker = {
    enable = true;
    storageDriver = "zfs";
  };
  virtualisation.libvirtd.enable = true;
  users.extraGroups.vboxusers.members = [ "zjn" ];

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
    openssh.authorizedKeys.keyFiles = (import ./net/keys.nix);
    hashedPassword =
      "$6$O1ia1YA5iKh9m$SVD17ySqqyicSpo2tzqTw4xRHm8C50.vMuoQPaLxTA9hsfJ7HQ/neioEYhOjZvPT..HNclbjd4JX4ydBcMvC7.";
  };
  users.groups.zjn = { };
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
    script = ''
      set -eux
      mkdir -p /cache/zjn /persist/zjn /persist/ssh
      chown zjn:users /cache/zjn /persist/zjn
    '';
    serviceConfig = { Type = "oneshot"; };
  };

  users.extraGroups.zfs.members = [ "zjn" ];
  systemd.services.zfsPerms = {
    description = "Set up ZFS permissions.";
    wantedBy = [ "multi-user.target" ];
    before = [ "znapzend.service" ];
    path = [ "/run/current-system/sw/" ];
    script = ''
      zfs allow -g zfs create,destroy,mount,receive,userprop tank/backups
    '';
    serviceConfig = { Type = "oneshot"; };
  };
  services.znapzend = {
    enable = true;
    autoCreation = true;
    pure = true;
    zetup = {
      "tank/safe" = {
        plan = "1h=>10min,1d=>1h,1m=>1d,1y=>1m";
        recursive = true;
        destinations = builtins.listToAttrs (builtins.map (x: {
          name = "${x}";
          value = {
            host = "zjn@${x}";
            dataset = "tank/backups/${config.networking.hostName}";
          };
        }) hosts);
      };
    };

  };
}
