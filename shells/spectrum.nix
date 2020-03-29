let
  moz_overlay = import (builtins.fetchTarball https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz);
  nixpkgs = import <nixpkgs> { overlays = [ moz_overlay ]; };
  rustNightlyChannel = (nixpkgs.rustChannelOf { date = "2020-03-19"; channel = "nightly"; }).rust.override {
    extensions = [
      "rust-src"
      "rls-preview"
      "clippy-preview"
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
    rustNightlyChannel
    rustracer
    protobuf
    gnum4
    etcd
    openssl
    pkgconfig
  ];

  PROTOC = "${pkgs.protobuf}/bin/protoc";

}
