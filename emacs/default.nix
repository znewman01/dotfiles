{ config, pkgs, lib, ... }:

{
  home.packages = with pkgs; [ emacs-all-the-icons-fonts ispell ];

  programs.doom-emacs = {
    enable = true;
    doomPrivateDir = (import ./doom.d) {
      inherit lib;
      inherit (pkgs) stdenv emacs coreutils;
    };
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
