{ pkgs, lib, ... }:

{
  services.syncthing.enable = pkgs.stdenv.isLinux;
}
