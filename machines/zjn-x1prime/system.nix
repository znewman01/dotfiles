{ config, pkgs, lib, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  networking.hostName = "zjn-x1prime";
  networking.hostId = "e2102671";
  system.stateVersion = "20.09";

  # For DisplayLink stuff: must be >5.10
  boot.kernelPackages = pkgs.linuxPackages_5_11;

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

  # To get DisplayLink (USB video for dock at work) working
  services.xserver.videoDrivers = [ "displaylink" "modesetting" ];

  networking.wireguard.interfaces.wg0 = {
    ips = [ "10.100.0.2/24" ];
    peers = [{ # zjn-workstation
      publicKey = "RCRTXUu4hPEoxzIvry0bnBtjnnK/2KuhwHEUPWSKZjI=";
      allowedIPs = [ "10.100.0.0/24" ];
      endpoint = "zjn-workstation.csail.mit.edu:51820";
    }];
  };
}
