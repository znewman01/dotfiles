{ lib, pkgs, ... }:

let
  pname = "bors";
  version = "HEAD";
  name = "${pname}-${version}";

  beamPackages = pkgs.beam.packagesWith pkgs.beam.interpreters.erlang;

  src = builtins.fetchGit {
    url = "https://github.com/bors-ng/bors-ng";
    rev = "7ea69fcf640c0c3cff3a7cbc3ac492471329e8c5";
  };

  # Dependencies
  mixEnv = "prod";
  mixDeps = beamPackages.fetchMixDeps {
    pname = "mix-deps-${pname}";
    inherit src mixEnv version;
    ALLOW_PRIVATE_REPOS = "true";
    sha256 = "19g9na551qyv1djdipxxna9zxr3mshizq6kmqym57jjxhbg7ljh6";
  };
  nodeEnv = (pkgs.callPackage ./node-env.nix { });
  nodeDependencies = (pkgs.callPackage ./node-packages.nix {
    inherit nodeEnv;
    src = "${src}/assets";
  }).shell.nodeDependencies;
  nodePackages = nodeDependencies // {
    "phoenix-../deps/phoenix" =
      nodeDependencies."phoenix-../deps/phoenix".override {
        src = "${mixDeps}/phoenix";
      };
    "phoenix_html-../deps/phoenix_html" =
      nodeDependencies."phoenix_html-../deps/phoenix_html".override {
        src = "${mixDeps}/phoenix_html";
      };
  };

  frontEndFiles = pkgs.stdenvNoCC.mkDerivation {
    pname = "frontend-${pname}";

    nativeBuildInputs = [ pkgs.nodejs ];

    inherit version src;

    buildPhase = ''
      cp -r ./assets $TEMPDIR

      mkdir -p $TEMPDIR/assets/node_modules/.cache
      cp -r ${nodePackages}/lib/node_modules $TEMPDIR/assets
      export PATH="${nodePackages}/bin:$PATH"

      cd $TEMPDIR/assets
      sed -i 's,"file:../deps/phoenix","file:${nodePackages}/bors/node_modules/phoenix",' package.json
      sed -i 's,"file:../deps/phoenix_html","file:${nodePackages}/bors/node_modules/phoenix_html",' package.json
      webpack --config ./webpack.config.js
      cd ..
    '';

    installPhase = ''
      cp -r ./priv/static $out/
    '';

    outputHashAlgo = "sha256";
    outputHashMode = "recursive";
    outputHash = "0csmli0vj0a7jwigr8sfq12smssyl82cnrc4s9pdm7y02p071fpn";

    impureEnvVars = lib.fetchers.proxyImpureEnvVars;
  };

in beamPackages.mixRelease {
  inherit src pname version mixDeps mixEnv;
  ALLOW_PRIVATE_REPOS = "true";
  postBuild = ''
    mix do deps.loadpaths --no-deps-check, phx.digest ${frontEndFiles} -o ./priv/static
  '';
  preInstall = ''
    mkdir -p $out/priv/static
    cp -r priv/static/* $out/priv/static/
  '';
}
