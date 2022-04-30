{ config, pkgs, ... }:

let hosts = [ "zjn@zjn-x1prime" "zjn@zjn-home" "zjn@zjn-work" "zjn-cloud" ];
in {
  environment.systemPackages = with pkgs; [ zfs ];

  services.zfs.autoSnapshot.enable = true;
  services.zfs.autoScrub.enable = true;
  boot.zfs.enableUnstable = true;

  systemd.services.zfsPerms = {
    description = "Set up ZFS permissions.";
    wantedBy = [ "multi-user.target" ];
    before = [ "znapzend.service" ];
    path = [ "/run/current-system/sw/" ];
    script = ''
      zfs allow -g zfs create,destroy,mount,receive,userprop tank/encrypt/backups
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
      "tank/encrypt/safe" = {
        plan = "1h=>10min,1d=>1h,1m=>1d,1y=>1m";
        recursive = true;
        destinations = builtins.listToAttrs (builtins.map (host: {
          name = pkgs.lib.lists.last (builtins.split "@" host);
          value = {
            host = host;
            dataset = "tank/encrypt/backups/${config.networking.hostName}";
          };
        }) hosts);
      };
    };
  };

  users.extraGroups.zfs.members = [ "zjn" ];

  virtualisation.docker.storageDriver = "zfs";
}
