{ config, lib, pkgs, ... }:

let
  unstableTarball20200811 = builtins.fetchTarball {
    name = "nixos-unstable-2020-08-11";
    url =
      "https://github.com/nixos/nixpkgs/archive/5c68e3171e457c007a96520a0008f7a131eb2c13.tar.gz";
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "0lmwsvprmb6fyzmcfzwfshj32m96z3g77adnk9sq53ikv2pkx9la";
  };
  dag = config.lib.dag;
  colors = (import ./../desktop/colors.nix) { lib = lib; };
in {
  nixpkgs.config = {
    packageOverrides = pkgs: {
      unstable20200811 =
        import unstableTarball20200811 { config = config.nixpkgs.config; };
    };
  };

  programs.emacs = {
    package = with pkgs; unstable20200811.emacs;
    extraPackages = epkgs: [ epkgs.use-package ];
    enable = true;
  };
  services.emacs.enable = true;

  home.file.".doom.d/themes/base16-zjn-theme.el".text = ''
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

  home.file.".emacs.d.template" = {
    source = pkgs.fetchFromGitHub {
      owner = "hlissner";
      repo = "doom-emacs";
      # git ls-remote https://github.com/hlissner/doom-emacs/ develop
      rev = "fd7073240c5f71ba64b9b990f840edc4d7c52138";
      # just rerun with the all-0 SHA, it'll tell you what to put
      # (but if it matches a previous SHA it won't update!)
      sha256 = "10lgzvhv6g9hmqzamphrfw9p37xvvp7kzr5zb90c8w3k7lmk11cp";
    };
    onChange = ''
      # Hack to prevent re-syncing unless doom is updated.
      # .emacs.d gets e.g., compiled files in it, so it's not expected to
      # match the source.
      mkdir -p ~/.emacs.d
      rsync \
          --itemize-changes \
          --links \
          --recursive \
          --checksum \
          ~/.emacs.d.template/ ~/.emacs.d/
      EMACSDIR=~/.emacs.d ~/.emacs.d/bin/doom sync
    '';
  };

  programs.bash.sessionVariables."EMACSDIR" = "~/.emacs.d";

  # # Use a link rather than home.files because we probably want to be able to
  # # hack on this pretty sloppily
  home.links.".doom.d" = "git/dotfiles/emacs/doom.d";
  home.links."notes" = "Dropbox/notes";
}
