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
      # git ls-remote https://github.com/syl20bnr/spacemacs/ develop
      rev = "332f1bb06fcf9b8410c47122d03122f3e7056106";
      sha256 = "1sv6q2232nh5f20rbi1dfpkmjqqv4ajwfx81rar0k5qch7pc4y1y";
    };
  };

  # Use a link rather than home.files because we probably want to be able to
  # hack on this pretty sloppily
  home.links.".spacemacs.d" = "dotfiles/emacs/spacemacs.d";
  home.links."notes" = "Dropbox/notes";
}
