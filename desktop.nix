{ config, pkgs, ... }:

let hosts = [ "zjn@zjn-x1prime" "zjn@zjn-home" "zjn@zjn-work" "zjn-cloud" ];
in {
  imports = [ ./desktop/system.nix ./persist/system.nix ];

  nix.trustedUsers = [ "@wheel" ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  programs.dconf.enable = true;
  services.dbus.packages = with pkgs; [ gnome3.dconf ];

  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };
  fonts.fontconfig.enable = true;

  time.timeZone = "America/New_York";

  virtualisation.docker.storageDriver = "zfs";
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
  # TODO: move to home.nix when rycee/home-manager#1087 resolved
  # https://github.com/rycee/home-manager/issues/1087
  programs.ssh.startAgent = true;
  services.openssh.permitRootLogin = "no";

  security.pam.loginLimits = [{
    domain = "*";
    type = "soft";
    item = "nofile";
    value = "2048"; # the default of 1024 makes some software (e.g., Rust) choke
  }];

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

  # TODO: skip yourself!
  services.znapzend = {
    enable = true;
    autoCreation = true;
    pure = true;
    features = { recvu = true; };
    zetup = {
      "tank/safe" = {
        plan = "1h=>10min,1d=>1h,1m=>1d,1y=>1m";
        recursive = true;
        destinations = builtins.listToAttrs (builtins.map (host: {
          name = pkgs.lib.lists.last (builtins.split "@" host);
          value = {
            host = host;
            dataset = "tank/backups/${config.networking.hostName}";
          };
        }) hosts);
      };
    };
  };
}
