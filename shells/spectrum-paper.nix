with import <nixpkgs> { };

let
  tex-env = texlive.combine { inherit (texlive) scheme-full latexmk; };
  pythonPackages = ps:
    with ps; [
      numpy
      matplotlib
      pandas

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

    # plots
    python3
    (python3.withPackages pythonPackages)

    # data preprocessing
    units
    jq
    envsubst
  ];
}
