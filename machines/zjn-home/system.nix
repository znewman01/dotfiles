{ config, pkgs, lib, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  # ZFS broken in 5.12
  boot.kernelPackages = pkgs.linuxPackages_5_10;

  networking.hostName = "zjn-home";
  system.stateVersion = "20.09";

  networking.wireguard.interfaces.wg0 = {
    ips = [ "10.100.0.3/24" ];
    peers = [{ # zjn-workstation
      publicKey = "RCRTXUu4hPEoxzIvry0bnBtjnnK/2KuhwHEUPWSKZjI=";
      allowedIPs = [ "10.100.0.0/24" ];
      endpoint = "zjn-workstation.csail.mit.edu:51820";
    }];
  };
}
