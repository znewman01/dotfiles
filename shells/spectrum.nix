let
  moz_overlay = import (builtins.fetchTarball https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz);
  nixpkgs = import <nixpkgs> { overlays = [ moz_overlay ]; };
  rustChannel = (nixpkgs.rustChannelOf { date = "2020-08-27"; channel = "nightly"; }).rust.override {
    extensions = [
      "rust-src"
      "rls-preview"
      "rust-analysis"
      "clippy-preview"
      "rustfmt-preview"
    ];
  };
  pythonPackages = ps: with ps; [
    tenacity
    asyncssh
    # Linting
    black
    pylint
    # For development ergonomics
    ipython
    # graphs
    matplotlib
    numpy
    pandas
  ];

in
with nixpkgs;
stdenv.mkDerivation {
  name = "rust-env";
  buildInputs = [
    # Build requirements
    gmp6
    stdenv
    rustChannel
    protobuf
    glibc
    gnum4
    openssl
    libffi
    pkgconfig
    # local testing
    etcd
    # DevOps
    packer
    terraform
    # Python
    python3
    (python3.withPackages pythonPackages)
    nodePackages.pyright
  ];

  PROTOC = "${pkgs.protobuf}/bin/protoc";
  RUST_SRC_PATH = "${rustChannel}/lib/rustlib/src/rust/src";

  shellHook = ''
    export PATH="$PATH:/home/zjn/.cargo/bin"
  '';

}
