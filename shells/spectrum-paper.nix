with import <nixpkgs> { };

let
  tex-env = texlive.combine {
    inherit (texlive)
      scheme-medium
      preprint # for authblk
      tabu
      varwidth
      xargs
      forloop
      pbox
      bigfoot # for suffix
      environ
      trimspaces
      was # for upgreek
      latexmk;
  };
in pkgs.mkShell rec {
  buildInputs = [
    tex-env
    gnumake
  ];
}
