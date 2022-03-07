{ config, pkgs, ... }:

{
  networking.firewall.allowedUDPPorts = [
    41641 # tailscale
  ];
  services.tailscale.enable = true;
  networking.firewall.trustedInterfaces = [ "tailscale0" ];

  environment.systemPackages = with pkgs; [ tailscale ];
}

