with import <nixpkgs> { };

let
  pythonPackages = ps: with ps; [
    attrs
    beautifulsoup4
    html5lib
    requests
    # Testing
    tox
    pytest
    responses
    parameterized
    # Linting
    black
    mypy
    pylint
  ];
in pkgs.mkShell rec {
  buildInputs = with pkgs; [
    (python36.withPackages pythonPackages)
    nodePackages.pyright
    stdenv
  ];
}
