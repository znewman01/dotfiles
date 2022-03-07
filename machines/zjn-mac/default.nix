{ config, pkgs, lib, inputs, ... }:

{
  networking.hostName = "zjn-mac";
  system.stateVersion = 4;
  
  environment.systemPackages = with pkgs; [ git ];

  services.nix-daemon.enable = true;
  nix.useDaemon = true;
  nix.package = pkgs.nix_2_4;
  nix.extraOptions = ''
    extra-platforms = aarch64-darwin x86_64-darwin
    experimental-features = nix-command flakes
  '';
}
