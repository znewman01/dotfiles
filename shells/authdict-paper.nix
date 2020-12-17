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
      enumitem
      soul
      booktabs
      threeparttable
      libertine
      glossaries
      glossaries-extra
      forest
      pgfopts
      environ
      trimspaces
      inlinedef
      mfirstuc
      xfor
      datatool
      fnpct
      titlesec
      titling
      translations
      IEEEtran
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
