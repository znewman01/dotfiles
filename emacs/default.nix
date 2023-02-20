{ config, pkgs, lib, ... }:

{
  home.packages = with pkgs;
    [ emacs-all-the-icons-fonts ispell zstd coreutils ]
    ++ (lib.optionals pkgs.stdenv.isLinux [ xorg.xwininfo ]);

  programs.doom-emacs = rec {
    enable = true;
    emacsPackage = pkgs.emacs;
    doomPrivateDir = (import ./doom.d) {
      inherit lib;
      inherit (pkgs) stdenv coreutils;
      emacs = emacsPackage;
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

  services.emacs = lib.optionalAttrs pkgs.stdenv.isLinux {
    enable = true;
    socketActivation.enable = true;
  };

  home.file.".doom-themes/fonts.el" = lib.optionalAttrs pkgs.stdenv.isLinux {

    text = ''
      (when (eq system-type 'gnu/linux)
        (setq zjn--mono "${
          (builtins.head config.fonts.terminalFonts).name
        }") ;; TODO: get these from system fonts module!
        (setq zjn--sans "Bitstream Vera Sans")
        (setq zjn--serif "TeX Gyre Pagella")
        (setq doom-font (font-spec :family zjn--mono :height 80 :weight 'semi-light))
        (setq doom-variable-pitch-font (font-spec :family zjn--serif :height 60)))
    '';
  };
  home.file.".doom-themes/base16-zjn-theme.el" =
    lib.optionalAttrs pkgs.stdenv.isLinux {
      text = let colors = config.colorScheme.colors;
      in ''
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
    };

  # xdg.mimeApps = {
  #   enable = true;
  #   defaultApplications = { "application/pdf" = "emacsclient.desktop"; };
  # };
  # xdg.dataFile."applications/emacsclient.desktop".text = ''
  #   [Desktop Entry]
  #   Name=EmacsClient
  #   GenericName=Text Editor
  #   Comment=Edit text
  #   MimeType=text/english;text/plain;text/x-makefile;text/x-c++hdr;text/x-c++src;text/x-chdr;text/x-csrc;text/x-java;text/x-moc;text/x-pascal;text/x-tcl;text/x-tex;application/x-shellscript;text/x-c;text/x-c++;
  #   Exec=em %F
  #   Icon=emacs
  #   Type=Application
  #   Terminal=false
  #   Categories=Development;TextEditor;
  #   StartupWMClass=Emacs
  #   Keywords=Text;Editor;
  # '';
}
