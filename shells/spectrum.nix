let
  moz_overlay = import (builtins.fetchTarball https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz);
  nixpkgs = import <nixpkgs> { overlays = [ moz_overlay ]; };
  rustChannel = (nixpkgs.rustChannelOf { date = "2020-04-10"; channel = "nightly"; }).rust.override {
    extensions = [
      "rust-src"
      "rls-preview"
      "rust-analysis"
      "clippy-preview"
      "rustfmt-preview"
    ];
  };
  pythonPackages = ps: with ps; [
    # Linting
    black
    mypy
    pylint
    # For development ergonomics
    python-language-server
    ipython
  ];
in
with nixpkgs;
stdenv.mkDerivation {
  name = "rust-env";
  buildInputs = [
    gmp6
    stdenv
    rustChannel
    protobuf
    gnum4
    etcd
    openssl
    libffi
    pkgconfig
    packer
    terraform
    python3
    (python3.withPackages pythonPackages)
  ];

  PROTOC = "${pkgs.protobuf}/bin/protoc";
  RUST_SRC_PATH = "${rustChannel}/lib/rustlib/src/rust/src";

  shellHook = ''
    export PATH="$PATH:/home/zjn/.cargo/bin"
  '';

}
