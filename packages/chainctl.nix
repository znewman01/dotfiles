{ fetchurl, stdenv, lib }:

let
  srcDarwin = fetchurl {
    url = "https://dl.enforce.dev/chainctl_Darwin_arm64";
    sha256 = "sha256-SyAAD0PBKEKzJiTpJkOSPJ+wHLVzjGX3l7zPChrudVg";
  };
  srcX86_64 = fetchurl {
    url = "https://dl.enforce.dev/chainctl_Linux_x86_64";
    sha256 = "sha256-Jg7cw6Mz0H9TxiUQNNJ9WT7EvceZu8pXb8gB9TZ7trs=";
  };
in stdenv.mkDerivation {
  pname = "chainctl";
  version = "2edb4df";

  src = if stdenv.isDarwin then srcDarwin else srcX86_64;

  unpackPhase = "true";

  buildPhase = "true";

  installPhase = ''
    mkdir -p $out/bin
    cp $src $out/bin/chainctl
    chmod +x $out/bin/chainctl
  '';

  meta = with lib; {
    homepage =
      "https://edu.chainguard.dev/chainguard/chainguard-enforce/chainctl-docs/";
    description = "CLI tool for Chainguard Enforce";
    # license = licenses.unfree;
  };
}
