# https://github.com/nix-community/nix-doom-emacs/issues/60#issuecomment-1083630633
{ version ? "dev", lib, stdenv, emacs, coreutils }:

stdenv.mkDerivation {
  pname = "emacs-config";
  inherit version;
  src = lib.sourceByRegex ./. [ "config.org" "init.el" "packages.el" ];

  buildInputs = [ emacs coreutils ];
  buildPhase = ''
    cp $src/* .
    # Tangle org files
    emacs --batch -Q \
      -l org \
      config.org \
      -f org-babel-tangle
  '';

  dontUnpack = true;

  installPhase = ''
    install -D -t $out *.el
  '';
}
