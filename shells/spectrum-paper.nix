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
  pythonPackages = ps: with ps; [
    python-language-server
    numpy
    matplotlib
    # Testing
    nose2
    # Linting
    black
    mypy
    pylint
    # For development ergonomics
    ipython
  ];
in pkgs.mkShell rec {
  buildInputs = [
    tex-env
    gnumake
    tectonic
    python3
    (python3.withPackages pythonPackages)
  ];
}
