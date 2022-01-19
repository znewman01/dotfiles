{ config, pkgs, ... }:

let badhosts = /home/zjn/Sync/documents/badhosts.txt;
in {
  networking.extraHosts = if builtins.pathExists badhosts then
    builtins.readFile badhosts else "";
}
