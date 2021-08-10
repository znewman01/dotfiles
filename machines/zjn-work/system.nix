{ config, pkgs, lib, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  # ZFS broken in 5.12
  boot.kernelPackages = pkgs.linuxPackages_5_10;

  networking.hostName = "zjn-work";
  networking.domain = "csail.mit.edu";
  # networking.interfaces.enp0s31f6.useDHCP = false;
  # networking.interfaces.enp0s31f6.ipv4.addresses = [{
  #   address = "128.30.93.6";
  #   prefixLength = 23;
  # }];
  # networking.defaultGateway = "128.30.92.1";
  networking.nameservers = [ "8.8.8.8" ];
  system.stateVersion = "21.05";

  # wireguard
  # networking.nat.enable = true;
  # networking.nat.externalInterface = "enp0s31f6";
  # networking.nat.internalInterfaces = [ "wg0" ];

  networking.wireguard.interfaces.wg0 = {
    ips = [ "10.100.0.4/24" ];
    peers = [{ # zjn-workstation
      publicKey = "RCRTXUu4hPEoxzIvry0bnBtjnnK/2KuhwHEUPWSKZjI=";
      allowedIPs = [ "10.100.0.0/24" ];
      endpoint = "zjn-workstation.csail.mit.edu:51820";
    }];
  };
}
