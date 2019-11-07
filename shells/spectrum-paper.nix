with import <nixpkgs> { };

let
  tex-env = texlive.combine {
    inherit (texlive)
      scheme-medium
      latexmk;
  };
in pkgs.mkShell rec {
  buildInputs = [
    tex-env
    gnumake
    tectonic
  ];
}
