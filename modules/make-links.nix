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
        # - if target is link pointing at the right place, do nothing
        # - if target exists and is not link, error
        # - if target is link pointing at the wrong place, fix it
        # - if target doesn't exist, make it
      in ''
        if [ ! -h "${target}" ]; then
          $DRY_RUN_CMD ln -snT "${source}" "${target}"
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
