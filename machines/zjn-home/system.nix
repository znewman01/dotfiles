{ config, pkgs, lib, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  boot.kernelPackages = pkgs.linuxPackages_5_10;

  networking.hostName = "zjn-home";
  system.stateVersion = "20.09";

  networking.wireguard.interfaces.wg0 = {
    ips = [ "10.100.0.3/24" ];
    peers = [{ # zjn-work
      publicKey = "BEbArNy/1PXq7ajme3ENJiAUGdZrr93IhnCwLTNBSE0=";
      allowedIPs = [ "10.100.0.0/24" ];
      endpoint = "zjn-work.csail.mit.edu:51820";
    }];
  };
}
