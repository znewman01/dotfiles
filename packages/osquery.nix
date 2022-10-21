{ fetchurl, stdenv, autoPatchelfHook, dpkg, zlib }:

stdenv.mkDerivation rec {
  pname = "osquery";
  version = "5.5.1";
  src = fetchurl {
    url =
      "https://github.com/osquery/osquery/releases/download/${version}/osquery_${version}-1.linux_amd64.deb";
    sha256 = "sha256-Ahddo6jQvx3sqrR8c4X2VkNv1nmNdTczRa4gq6eKDQE=";
  };
  sourceRoot = ".";
  unpackCmd = "${dpkg}/bin/dpkg-deb -x $curSrc .";
  nativeBuildInputs = [ autoPatchelfHook ];
  buildInputs = [ zlib ];
  installPhase = ''
    cp -r opt/osquery $out
  '';
}
