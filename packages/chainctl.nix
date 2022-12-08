{ fetchurl, stdenv, lib }:

let
  srcDarwin = fetchurl {
    url = "https://dl.enforce.dev/chainctl_Darwin_arm64";
    sha256 = "sha256-BXQupzzEu7zexSNTAEzcqEYRzC8XIoP8vi7Yzj7qdpk";
  };
  srcX86_64 = fetchurl {
    url = "https://dl.enforce.dev/chainctl_Linux_x86_64";
    sha256 = "sha256-h6YqxnFct9qow4OvtHX/E13G7YCyf8rwj6NksqdOKfk=";
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
