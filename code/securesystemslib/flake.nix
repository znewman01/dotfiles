{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { nixpkgs, flake-utils, poetry2nix, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ poetry2nix.overlay ];
        };
        noCheck = _: { doCheck = false; };
        pythonPackages = pkgs.python310Packages;
        checkPythonPackages = ps: with ps; [ tox mypy coverage pytest ];
        runPythonPackages = ps: with ps; [ cryptography colorama pynacl ];
      in rec {
        packages.default = pythonPackages.buildPythonPackage {
          src = ./.;
          name = "securesystemslib";
          version = "0.24.0";
          checkInputs = [ (pkgs.python310.withPackages checkPythonPackages) ]
            ++ [ pkgs.gnupg ];
          propagatedBuildInputs = runPythonPackages pythonPackages;
          checkPhase = ''
            runHook preCheck

            python tests/aggregate_tests.py

            runHook postCheck
          '';
        };
        devShells.default = pkgs.mkShell {
          TOX_SKIP_ENV = "py37";
          buildInputs =
            let makePyEnv = python: python.withPackages runPythonPackages;
            in [
              pkgs.nixfmt
              pkgs.gnupg
              (pkgs.python310.withPackages (ps:
                (with ps; [ pylint isort ]) ++ (checkPythonPackages ps)
                ++ (runPythonPackages ps)))
              pkgs.python38
              pkgs.python39
            ];
        };
      });
}
