# https://github.com/nix-community/nix-doom-emacs/issues/60#issuecomment-1083630633
{ version ? "dev", lib, stdenv, emacs, coreutils }:

let
in stdenv.mkDerivation {
  pname = "emacs-config";
  inherit version;
  src = ./.;

  buildInputs = [ emacs coreutils ];
  buildPhase = ''
    cp -r $src/* .
    # Tangle org files
    emacs --batch -Q \
      -l org \
      config.org \
      -f org-babel-tangle
  '';

  dontUnpack = true;

  installPhase = ''
    mkdir -p $out
    cp -r *.el snippets/ $out/
  '';
}
