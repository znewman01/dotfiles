{ config, pkgs, ... }:

let
  tex-env =
    pkgs.texlive.combine { inherit (pkgs.texlive) scheme-full latexmk; };
in {
  home.packages = with pkgs; [
    tex-env
  ];
}
