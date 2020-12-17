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


  # home.file.".emacs.d" = {
  #   recursive = true;
  #   source = pkgs.fetchFromGitHub {
  #     owner = "syl20bnr";
  #     repo = "spacemacs";
  #     # git ls-remote https://github.com/syl20bnr/spacemacs/ develop
  #     rev = "332f1bb06fcf9b8410c47122d03122f3e7056106";
  #     sha256 = "1sv6q2232nh5f20rbi1dfpkmjqqv4ajwfx81rar0k5qch7pc4y1y";
  #   };
  # };

  # # Use a link rather than home.files because we probably want to be able to
  # # hack on this pretty sloppily
  # home.links.".spacemacs.d" = "dotfiles/emacs/spacemacs.d";
  # home.links."notes" = "Dropbox/notes";
}
