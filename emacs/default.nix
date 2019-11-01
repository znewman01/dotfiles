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
      rev = "26b8fe0c317915b622825877eb5e5bdae88fb2b2";
      sha256 = "00cfm6caaz85rwlrbs8rm2878wgnph6342i9688w4dji3dgyz3rz";
    };
  };

  # Use a link rather than home.files because we probably want to be able to
  # hack on this pretty sloppily
  home.links.".spacemacs.d" = "dotfiles/emacs/spacemacs.d";
  home.links."notes" = "Dropbox/notes";
}
