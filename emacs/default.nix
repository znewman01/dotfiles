{ config, lib, pkgs, ... }:

let
  dag = config.lib.dag;
  colors = (import ./../desktop/colors) { lib = lib; };
in {
  home.packages = with pkgs; [ emacs-all-the-icons-fonts ];

  programs.emacs = {
    package = pkgs.emacs;
    extraPackages = epkgs: [ epkgs.use-package ];
    enable = true;
  };
  services.emacs.enable = true;

  home.file.".doom-themes/base16-zjn-theme.el".text = ''
     (require 'base16-theme)
     (defvar base16-zjn-colors
       '(:base00 "#${colors.base00}"
         :base01 "#${colors.base01}"
         :base02 "#${colors.base02}"
         :base03 "#${colors.base03}"
         :base04 "#${colors.base04}"
         :base05 "#${colors.base05}"
         :base06 "#${colors.base06}"
         :base07 "#${colors.base07}"
         :base08 "#${colors.base08}"
         :base09 "#${colors.base09}"
         :base0A "#${colors.base0A}"
         :base0B "#${colors.base0B}"
         :base0C "#${colors.base0C}"
         :base0D "#${colors.base0D}"
         :base0E "#${colors.base0E}"
         :base0F "#${colors.base0F}")
     "All colors for Base16 zjn are defined here.")
    (deftheme base16-zjn)
    (base16-theme-define 'base16-zjn base16-zjn-colors)
    (provide-theme 'base16-zjn)
    (provide 'base16-zjn-theme)
  '';

  home.file."bin/em" = {
    text = ''
      #!/bin/sh
      emacsclient -nc $@
    '';
    executable = true;
  };
  xdg.mimeApps = {
    enable = true;
    defaultApplications = { "application/pdf" = "emacsclient.desktop"; };
  };
  xdg.dataFile."applications/emacsclient.desktop".text = ''
    [Desktop Entry]
    Name=EmacsClient
    GenericName=Text Editor
    Comment=Edit text
    MimeType=text/english;text/plain;text/x-makefile;text/x-c++hdr;text/x-c++src;text/x-chdr;text/x-csrc;text/x-java;text/x-moc;text/x-pascal;text/x-tcl;text/x-tex;application/x-shellscript;text/x-c;text/x-c++;
    Exec=em %F
    Icon=emacs
    Type=Application
    Terminal=false
    Categories=Development;TextEditor;
    StartupWMClass=Emacs
    Keywords=Text;Editor;
  '';

  home.file.".emacs.d.template/emacs.d" = let
    # git ls-remote https://github.com/hlissner/doom-emacs/ develop
    rev = "bc47e72984f9e6eb89a19e9560185188575b10c1";
  in {
    source = pkgs.fetchFromGitHub {
      owner = "hlissner";
      repo = "doom-emacs";
      rev = rev;
      sha256 = "0qhm5qrr2753xbrhvnb29rww36ln51z4skycrd5l4pqn3x3va7y1";
    };
    onChange = ''
      DST="$HOME/.emacs.d"
      mkdir -p "$DST"
      # Hack to prevent re-syncing unless doom is updated.
      # .emacs.d gets e.g., compiled files in it, so it's not expected to
      # match the source.
      if [ "$(cat ~/.emacs.d/.rev)" != "${rev}" ]; then
        rsync \
            --itemize-changes \
            --links \
            --recursive \
            --checksum \
            ~/.emacs.d.template/emacs.d/ "$DST"
        echo ${rev} > ~/.emacs.d/.rev
        $DST/bin/doom sync
      fi
    '';
  };

  home.file."bin/emacsmail" = {
    text = ''
      #!/usr/bin/env bash
      emacsclient -c --eval "(browse-url-mail \"$@\")"
    '';
    executable = true;
  };

  programs.bash.sessionVariables."EMACSDIR" = "~/.emacs.d";

  systemd.user.tmpfiles.rules = [
    "L %h/notes - - - - %h/Sync/notes"
    # Use a link rather than home.files because we probably want to be able to
    # hack on this pretty sloppily
    "L %h/.doom.d - - - - %h/git/dotfiles/emacs/doom.d"
  ];
}
