with import <nixpkgs> { };

let
  pythonPackages = ps: with ps; [
    attrs
    beautifulsoup4
    html5lib
    requests
    # Testing
    nose
    responses
    parameterized
    # Linting
    black
    mypy
    pylint
    # Development
    ipython
  ];
in pkgs.mkShell rec {
  buildInputs = with pkgs; [
    (python3.withPackages pythonPackages)
    stdenv
  ];
}
