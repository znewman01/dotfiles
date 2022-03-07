{ config, pkgs, ... }:

{
  nix.extraOptions = "experimental-features = nix-command flakes";

  boot.cleanTmpDir = true;

  networking.firewall.allowedUDPPorts = [
    41641 # tailscale
  ];
  services.tailscale.enable = true;
  networking.firewall.trustedInterfaces = [ "tailscale0" ];

  i18n.defaultLocale = "en_US.UTF-8";

  environment.systemPackages = with pkgs; [
    manpages
    tailscale
    zfs
  ];

  services.openssh = {
    enable = true;
    passwordAuthentication = false;
  };
  users.users.root.openssh.authorizedKeys.keyFiles = (import ./net/keys.nix);

  virtualisation.docker.enable = true;

  services.zfs.autoSnapshot.enable = true;
  services.zfs.autoScrub.enable = true;
  boot.zfs.enableUnstable = true;

  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 30d";
  };
}
