with import <nixpkgs> { };

let
  tex-env = texlive.combine {
    inherit (texlive)
      scheme-medium
      Asana-Math
      stmaryrd;
  };
  pythonPackages = ps: with ps; [
    python-language-server
    numpy
    matplotlib
    seaborn
    pandas
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
    entr
    python3
    (python3.withPackages pythonPackages)
  ];
}
