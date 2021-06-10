{ config, pkgs, lib, ... }:

{
  networking.hostName = "zjn-x1prime";
  networking.hostId = "e2102671";
  system.stateVersion = "20.09";
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
    "L /etc/nixos/configuration.nix - - - - /persist/zjn/git/dotfiles/configuration.nix"
    "L /etc/nixos/hardware-configuration.nix - - - - /persist/hardware-configuration.nix"
    "L /etc/NetworkManager/system-connections - - - - /persist/nm-system-connections"
  ];
  # TODO: factor some of above out?

  environment.systemPackages = with pkgs; [ sof-firmware ];
  hardware.pulseaudio.extraConfig = ''
    load-module module-alsa-sink device=;w:0,0 channels=4
    load-module module-alsa-source device=hw:0,6 channels=4
  '';

  # Bizarre hack to allow monitor
  services.udev.path = with pkgs; [ lshw ];
  services.udev.extraRules = ''
    ACTION=="change" \
    , ATTRS{vendor}=="0x8086" \
    , ATTRS{subsystem_vendor}=="0x17aa" \
    , ATTRS{subsystem_device}=="0x2292" \
    , RUN+="${pkgs.lshw}/bin/lshw"
  '';
}
