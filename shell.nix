with import <nixpkgs> { };

pkgs.mkShell rec {
  buildInputs = with pkgs; [ stdenv nixfmt git-crypt terraform ];
}
