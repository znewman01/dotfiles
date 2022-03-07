{ config, pkgs, ... }:

{
  homebrew.masApps = { Tailscale = 1475387142; };
  homebrew.brews = [ "tailscale" ];
}
