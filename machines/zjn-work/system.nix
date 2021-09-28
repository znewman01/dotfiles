{ config, pkgs, lib, ... }:

{
  imports = [ ./hardware-configuration.nix ./../../services/bors.nix ];

  # ZFS broken in 5.12
  boot.kernelPackages = pkgs.linuxPackages_5_10;

  networking.hostName = "zjn-work";
  networking.domain = "csail.mit.edu";
  networking.interfaces.eno1.useDHCP = false;
  networking.interfaces.eno1.ipv4.addresses = [{
    address = "128.30.93.17";
    prefixLength = 23;
  }];
  networking.defaultGateway = "128.30.92.1";
  networking.nameservers = [ "8.8.8.8" ];
  system.stateVersion = "21.05";

  # wireguard
  # networking.nat.enable = true;
  # networking.nat.externalInterface = "enp0s31f6";
  # networking.nat.internalInterfaces = [ "wg0" ];

  networking.wireguard.interfaces.wg0 = {
    ips = [ "10.100.0.1/24" ];
    peers = [
      { # zjn-x1prime
        publicKey = "aaPN21zthpwZ1T2wDJ1nMNqH9p2Y2ml8w4JSOjg3TBY=";
        allowedIPs = [ "10.100.0.2/32" ];
      }
      { # zjn-home
        publicKey = "MWgkaRpaHRAOPshJjyzZMj1btM92go/eP1/Phk8q+xc=";
        allowedIPs = [ "10.100.0.3/32" ];
      }
      { # zjn-work
        publicKey = "BEbArNy/1PXq7ajme3ENJiAUGdZrr93IhnCwLTNBSE0=";
        allowedIPs = [ "10.100.0.4/32" ];
      }
    ];
  };
}
