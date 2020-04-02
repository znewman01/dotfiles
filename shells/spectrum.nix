let
  moz_overlay = import (builtins.fetchTarball https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz);
  nixpkgs = import <nixpkgs> { overlays = [ moz_overlay ]; };
  rustChannel = (nixpkgs.rustChannelOf { date = "2020-02-05"; channel = "nightly"; }).rust.override {
    extensions = [
      "rust-src"
      "rls-preview"
      "rustfmt-preview"
    ];
  };
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
    pkgconfig
  ];

  PROTOC = "${pkgs.protobuf}/bin/protoc";
  RUST_SRC_PATH = "${rustChannel}/lib/rustlib/src/rust/src";

  shellHook = ''
    export PATH="$PATH:/home/zjn/.cargo/bin"
  '';

}
