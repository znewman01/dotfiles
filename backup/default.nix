{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    restic
  ];
}
