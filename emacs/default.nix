{ config, pkgs, lib, ... }:

{
  home.packages = with pkgs; [
    emacs-all-the-icons-fonts
    ispell
    zstd
    coreutils
  ];

  programs.doom-emacs = rec {
    enable = true;
    emacsPackage = pkgs.emacsGit;
    doomPrivateDir = (import ./doom.d) {
      inherit lib;
      inherit (pkgs) stdenv coreutils;
      emacs = pkgs.emacsGit;
    };
    # Only init/packages so we only rebuild when those change.
    doomPackageDir = pkgs.linkFarm "doom-packages-dir" [
      {
        name = "init.el";
        path = ./doom.d/init.el;
      }
      {
        name = "packages.el";
        path = ./doom.d/packages.el;
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

  home.file."notes".source = config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/Sync/notes";
}
