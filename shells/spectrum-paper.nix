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
      fnpct
      translations
      tabu
      IEEEtran
      varwidth
      tcolorbox
      environ
      etoolbox
      trimspaces
      xargs
      forloop
      bigfoot
      breakurl
      was
      sttools
      pbox
      ifoddpage
      relsize
      latexmk;
  };
in pkgs.mkShell rec {
  buildInputs = [
    tex-env
    gnumake
    tectonic
  ];
}
