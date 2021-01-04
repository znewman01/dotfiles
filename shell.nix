with import <nixpkgs> { };

pkgs.mkShell rec {
  buildInputs = with pkgs; [
    stdenv
    nixfmt
  ];
}
