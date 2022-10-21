{ buildGoModule, fetchFromGitHub, go-bindata, git }:

buildGoModule rec {
  pname = "kolide-launcher";
  version = "v0.11.26";
  src = fetchFromGitHub {
    owner = "kolide";
    repo = "launcher";
    rev = "adaddbd7ec1743f4fc020ced139853eb72836e19";
    hash = "sha256-yrGmHk32AsfCb0Hx4K84UYFG0l/9bxCiMjcHff0jN7k=";
  };
  patches = [ ./tuf.patch ./r11y.patch ./hardcoded-bins.patch ];
  vendorHash = "sha256-cCSeceWBq1Ktex+0jMgOCOJ07VIrfIzmOi1zphHXfPQ=";
  buildPhase = ''
    # `make deps`
    go run cmd/make/make.go -targets=install-tools
    go generate ./pkg/packagekit/... ./pkg/packaging/... ./pkg/osquery/tables/... ./pkg/augeas/...

    # TUF update requires network connection
    # go run cmd/make/make.go -targets=generate-tuf

    # `make`
    go run cmd/make/make.go -targets=launcher -linkstamp
    go run cmd/make/make.go -targets=osquery-extension.ext -linkstamp
  '';
  checkPhase = ""; # tests fail on nixos
  installPhase = ''
    mkdir -p $out/bin
    cp build/launcher $out/bin/
    cp build/osquery-extension.ext $out/bin/
  '';
  nativeBuildInputs = [ go-bindata git ];
  GIT_SHA = "0000000000000000000000000000000000000000";
  GIT_BRANCH = "main";
  GIT_VERSION = version;
}
