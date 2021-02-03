with import <nixpkgs> { };

let
  tex-env = texlive.combine {
    inherit (texlive)
      scheme-full
      latexmk;
  };
in pkgs.mkShell rec {
  buildInputs = [
    tex-env
    gnumake
    ipe
    python3
    poppler_utils  # for pdftotext
    inkscape  # for svg rendering
  ];
}
