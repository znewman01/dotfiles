{ pkgs ? import <nixpkgs> { }
, pkgsLinux ? import <nixpkgs> { system = "x86_64-linux"; } }:

let
  script = pkgsLinux.writeScriptBin "beeminder-lichess.sh"
    (builtins.readFile ./beeminder-lichess.sh);
in pkgs.dockerTools.buildImage {
  name = "lichess";
  runAsRoot = "mkdir /tmp";

  contents = with pkgsLinux; [ jq curl script cacert busybox ];
  config = {
    Cmd = [ "${script}/bin/beeminder-lichess.sh" ];
    Env = [ "CURL_CA_BUNDLE=/etc/ssl/certs/ca-bundle.crt" ];
  };
}
