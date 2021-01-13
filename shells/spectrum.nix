with import <nixpkgs> { };

let
  moz_overlay = import (builtins.fetchTarball
    "https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz");
  nixpkgs = import <nixpkgs> { overlays = [ moz_overlay ]; };
  rustChannel = (nixpkgs.rustChannelOf {
    date = "2020-12-22";
    channel = "nightly";
  }).rust.override {
    extensions =
      [ "rust-src" "rust-analysis" "clippy-preview" "rustfmt-preview" ];
  };
  pythonPackages = python38Packages;
in pkgs.mkShell rec {
  name = "spectrum-env";
  venvDir = "./.venv";
  buildInputs = [
    # Basic build requirements
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

    # DevOps -- for running experiments
    packer
    terraform

    # Python -- for experiment scripts
    pythonPackages.python
    # We need a venv because some Python dependencies aren't in nixpkgs.
    # This execute some shell code to initialize a venv in $venvDir before
    # dropping into the shell
    pythonPackages.venvShellHook
    # Linting + development
    pythonPackages.black
    # pythonPackages.pylint
    pythonPackages.ipython
    nodePackages.pyright
    # graphs
    pythonPackages.matplotlib
    pythonPackages.numpy
    pythonPackages.pandas
  ];

  PROTOC = "${pkgs.protobuf}/bin/protoc";
  RUST_SRC_PATH = "${rustChannel}/lib/rustlib/src/rust/library";

  # Run this command, only after creating the virtual environment
  postVenvCreation = ''
    unset SOURCE_DATE_EPOCH
    pip install -r experiments/requirements.txt
    pip install pylint
  '';

  postShellHook = ''
    export PATH="$PATH:/home/zjn/.cargo/bin"
    # allow pip to install wheels
    unset SOURCE_DATE_EPOCH
  '';

}
