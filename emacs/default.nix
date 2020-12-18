{ config, lib, pkgs, ... }:

let
  unstableTarball20200811 = builtins.fetchTarball {
  name = "nixos-unstable-2020-08-11";
  url = "https://github.com/nixos/nixpkgs/archive/5c68e3171e457c007a96520a0008f7a131eb2c13.tar.gz";
  # Hash obtained using `nix-prefetch-url --unpack <url>`
  sha256 = "0lmwsvprmb6fyzmcfzwfshj32m96z3g77adnk9sq53ikv2pkx9la";
};
  dag = config.lib.dag;
in
{
  nixpkgs.config = {
    packageOverrides = pkgs: {
      unstable20200811 = import unstableTarball20200811 {
        config = config.nixpkgs.config;
      };
    };
  };

  programs.emacs = {
    package = with pkgs; unstable20200811.emacs;
    extraPackages = epkgs: [ epkgs.use-package ];
    enable = true;
  };
  services.emacs.enable = true;

  home.file."bin/em" = {
    text = ''
      #!/bin/sh
      emacsclient -nc $@
    '';
    executable = true;
  };


  home.file.".emacs.d" = {
    recursive = true;
    source = pkgs.fetchFromGitHub {
      owner = "hlissner";
      repo = "doom-emacs";
      # git ls-remote https://github.com/hlissner/doom-emacs/ develop
      rev = "0c9256411d6bdb6cfc920bcce2216d99ff28a0c5";
      # just rerun with the all-0 SHA, it'll tell you what to put
      # (but if it matches a previous SHA it won't update!)
      sha256 = "1702vgjx1ry5ykglv1gh37aqwhmjdc80fr1p954576zs56xw4kyz";
    };
    onChange = "EMACSDIR=~/.emacs.d ~/.emacs.d/bin/doom sync";
  };

  programs.bash.sessionVariables."EMACSDIR" = "~/.emacs.d";

  # # Use a link rather than home.files because we probably want to be able to
  # # hack on this pretty sloppily
  home.links.".doom.d" = "dotfiles/emacs/doom.d";
  home.links."notes" = "Dropbox/notes";
}
