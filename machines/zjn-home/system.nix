{ config, pkgs, lib, ... }:

{
  # ZFS broken in 5.12
  boot.kernelPackages = pkgs.linuxPackages_5_10;

  networking.hostName = "zjn-home";
  networking.hostId = "2f4cf0b0";
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
