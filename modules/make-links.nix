{ config, lib, pkgs, ... }:

with lib;

let

  dag = config.lib.dag;
  cfg = config.home.links;
  homeDirectory = config.home.homeDirectory;

in

{

  options = {
    home.links = mkOption {
      type = with types; attrsOf string;
      default = {};
      example = { "link/target" = "link/source"; };
      description = "Extra symlinks to make (to non-managed files).";
    };
  };

  config = let

    generateLinkSh = targetName: sourceName: (
      let
        source = "${homeDirectory}/${sourceName}";
        target = "${homeDirectory}/${targetName}";
        isDir =
          let base = baseNameOf source;
              parent = dirOf source;
              type = (builtins.readDir parent).${base} or null;
          in source == /. || type == "directory";
        lnFlags = if isDir then "-snT" else "-s";
      in ''
        if [ ! -e "${target}" ]; then
          $DRY_RUN_CMD ln ${lnFlags} "${source}" "${target}"
        fi
      '');

  in

    {
      home.activation.makeLinks =
        dag.entryAfter [ "writeBoundary" ] ''
          ${concatStringsSep "\n" (mapAttrsToList generateLinkSh cfg)}
        '';
    };

}
