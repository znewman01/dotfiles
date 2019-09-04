with import <nixpkgs> { };

let
  tex-env = texlive.combine {
    inherit (texlive)
      scheme-medium
      blindtext
      cleveref
      xifthen
      ifmtarg
      preprint  # for authblk
      mwe
      todonotes
      # For slideshow/org export
      wrapfig
      capt-of
      minted
      fvextra
      upquote
      ifplatform
      xstring
      framed
      # For building
      latexmk;
  };
  # For slideshow/org export
  pythonPackages = ps: with ps; [
      pygments
  ];
in pkgs.mkShell rec {
  buildInputs = [
    tex-env
    (python3.withPackages pythonPackages)
    gnumake
    ipe
    python3
    poppler_utils  # for pdftotext
    inkscape  # for svg rendering
  ];
}
