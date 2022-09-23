{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    gomod2nix.url = "github:tweag/gomod2nix";
    gomod2nix.inputs.nixpkgs.follows = "nixpkgs";
    nix-filter.url = "github:numtide/nix-filter";
  };
  outputs = { nixpkgs, flake-utils, gomod2nix, nix-filter, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ gomod2nix.overlays.default ];
        };
        python = pkgs.python310;
        securesystemslib = python.pkgs.buildPythonPackage rec {
          pname = "securesystemslib";
          version = "0.23.0";
          src = python.pkgs.fetchPypi {
            inherit pname version;
            sha256 = "sha256-Vz6cgQ8qav6axxoXfya51qMhxTV09WH1zvLtURw/GDE";
          };
          propagatedBuildInputs = with python.pkgs; [
            six
            pytest-subtests
            cryptography
            pynacl
            colorama
          ];
          doCheck = false;
        };
        python-tuf = python.pkgs.buildPythonPackage rec {
          pname = "tuf";
          version = "1.1.0";
          src = python.pkgs.fetchPypi {
            inherit pname version;
            sha256 = "sha256-KM016vpapCI+ugOjl9FKy1fFIjgRgNuf87VEd9y+G3M";
          };
          doCheck = false;
          propagatedBuildInputs = with python.pkgs;
            [ six iso8601 requests ] ++ [ securesystemslib ];
          prePatch = ''
            sed -i '/license =/d' pyproject.toml
          '';
        };
      in rec {
        packages.default = pkgs.buildGoApplication {
          pname = "go-tuf";
          version = "v0.0.0-deadbeef";
          src = nix-filter.lib.filter {
            root = ./.;
            exclude = [ (nix-filter.lib.matchExt "nix") ];
          };
          modules = ./gomod2nix.toml;
          checkInputs = [ python python-tuf ];
        };
        apps = {
          tuf = flake-utils.lib.mkApp {
            name = "tuf";
            drv = packages.default;
          };
          tuf-client = flake-utils.lib.mkApp {
            name = "tuf-client";
            drv = packages.default;
          };
        };
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs;
            [
              nixfmt
              gocode
              gore
              gomodifytags
              gopls
              go-symbols
              gopkgs
              go-outline
              gotests
              gotools
              golangci-lint
              gomod2nix.packages.${system}.default
              jq
            ] ++ packages.default.nativeBuildInputs;
        };
      });
}
