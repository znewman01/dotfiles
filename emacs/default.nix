{ config, pkgs, ... }:

{
  home.packages = with pkgs; [ emacs-all-the-icons-fonts ispell ];

  programs.doom-emacs = {
    enable = true;
    doomPrivateDir = ./doom.d;

    emacsPackagesOverlay = self: super: {
      # fixes https://github.com/vlaci/nix-doom-emacs/issues/394
      gitignore-mode = pkgs.emacsPackages.git-modes;
      gitconfig-mode = pkgs.emacsPackages.git-modes;
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
