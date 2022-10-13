{ config, pkgs, lib, ... }:

{
  home.packages = with pkgs; [ emacs-all-the-icons-fonts ispell ];

  programs.doom-emacs = rec {
    enable = true;
    doomPrivateDir = (import ./doom.d) {
      inherit lib;
      inherit (pkgs) stdenv emacs coreutils;
    };
    # Only init/packages so we only rebuild when those change.
    doomPackageDir = let
      filteredPath = builtins.path {
        path = doomPrivateDir;
        name = "doom-private-dir-filtered";
        filter = path: type:
          builtins.elem (baseNameOf path) [ "init.el" "packages.el" ];
      };
    in pkgs.linkFarm "doom-packages-dir" [
      {
        name = "init.el";
        path = "${filteredPath}/init.el";
      }
      {
        name = "packages.el";
        path = "${filteredPath}/packages.el";
      }
      {
        name = "config.el";
        path = pkgs.emptyFile;
      }
    ];
  };

  home.file."bin/em" = {
    text = ''
      #!/bin/sh
      emacsclient -nc $@
    '';
    executable = true;
  };

  home.file."bin/emacsmail" = {
    text = ''
      #!/usr/bin/env bash
      emacsclient -c --eval "(browse-url-mail \"$@\")"
    '';
    executable = true;
  };

  home.file."notes".source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/Sync/notes";
}
