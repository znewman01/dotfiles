{ config, pkgs, ... }:

{
  imports = [ ./default.nix ./tailscale/nixos.nix];

  boot.cleanTmpDir = true;

  i18n.defaultLocale = "en_US.UTF-8";

  environment.systemPackages = with pkgs; [ manpages ];

  services.openssh = {
    enable = true;
    passwordAuthentication = false;
  };
  users.users.root.openssh.authorizedKeys.keyFiles = (import ./net/keys.nix);

  virtualisation.docker.enable = true;

  nix.extraOptions = "experimental-features = nix-command flakes";
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 30d";
  };
}
