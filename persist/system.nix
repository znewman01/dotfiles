{ config, pkgs, lib, ... }:

let
  rev = "58558845bc68dcf2bb32caa80564f7fe3f6cbc61";
  impermanence = builtins.fetchTarball {
    # git ls-remote https://github.com/nix-community/impermanence master
    url = "https://github.com/nix-community/impermanence/archive/${rev}.tar.gz";
    # To update, make all-0 and run nixos-rebuild switch
    sha256 = "10z3g4knkvq838zbfq71pkfyl8cffrpavna448wf5mjscycp0gnv";
  };
in {
  imports = [ "${impermanence}/nixos.nix" ];

  programs.fuse.userAllowOther = true;
  boot.initrd.postDeviceCommands = lib.mkAfter ''
    zfs destroy -R tank/local/lastroot
    zfs rename tank/local/root tank/local/lastroot
    zfs set mountpoint=/tank/local/lastroot tank/local/lastroot
    zfs create -p -o mountpoint=legacy tank/local/root
  '';
  services.zfs.autoSnapshot.enable = true;
  services.openssh = {
    hostKeys = [{
      path = "/persist/ssh/ssh_host_ed25519_key";
      type = "ed25519";
    }];
  };
  systemd.tmpfiles.rules = [
    "L /etc/NetworkManager/system-connections - - - - /persist/nm-system-connections"
    # Needed for znapzend (so can SSH to other machines)
    "C /root/.ssh - - - - /persist/zjn/.ssh"
    "z /root/.ssh - root root - -"
  ];
  environment.persistence."/persist/root" = {
    directories = [ "/var/lib/acme" "/var/lib/tailscale" ];
  };

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
}
