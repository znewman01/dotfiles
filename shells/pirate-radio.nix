with import <nixpkgs> { };

pkgs.mkShell rec {
  buildInputs = with pkgs; [
    gmp6
    stdenv
    cargo
    rustc
    rustfmt
    gnum4
  ];
}
