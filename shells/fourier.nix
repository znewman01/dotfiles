with import <nixpkgs> { };

let
  pythonPackages = ps: with ps; [
    gmpy2  # requires nixos-unstable channel
    # Testing
    hypothesis
    nose2
    # Linting
    black
    mypy
    pylint
    # For development ergonomics
    ipython
  ];
in pkgs.mkShell rec {
  buildInputs = with pkgs; [
    stdenv
    # Library requirements
    gmp
    libmpc
    mpfr
    fftw
    # For testing (via Python)
    (python3.withPackages pythonPackages)
    # For linting
    clang-tools
  ];
}
