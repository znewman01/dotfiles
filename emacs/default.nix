{ config, lib, pkgs, ... }:

let
  dag = config.lib.dag;
in
{
  programs.emacs.enable = true;
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
      owner = "syl20bnr";
      repo = "spacemacs";
      # git ls-remote https://github.com/syl20bnr/spacemacs/ master
      rev = "8c0b8c3440a33df2c291088c47e75fd5e4fbbb61";
      sha256 = "1af73ls0znbpx73ym9cx22yc6q1dvv375l493ccilg2xpjsgam6w";
    };
  };

  home.links.".spacemacs.d" = "Dropbox/dotfiles/spacemacs.d";
  home.links."notes" = "Dropbox/notes";
}
