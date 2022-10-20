{ config, lib, pkgs, ... }:

with lib;

let myLib = import ./lib.nix { inherit pkgs lib; };
in with myLib; {
  code = {
    baseDir = "${config.home.homeDirectory}/git";
    repos = {
      "resume".url = myGH "resume";
      "dotfiles".url = myGH "dotfiles";
      "scalingsnapshots" = {
        url = myGH "scalingsnapshots";
        extraFiles."analysis/.projectile".text = "";
      };
      fulcio = pipe (gh "sigstore/fulcio") [ myFork forWork withDco ];
      rekor = pipe (gh "sigstore/rekor") [ myFork forWork withDco ];
      sigstore = pipe (gh "sigstore/sigstore") [ myFork forWork withDco ];
      cosign = pipe (gh "sigstore/cosign") [ myFork forWork withDco ];
      go-tuf = pipe (gh "theupdateframework/go-tuf") [
        myFork
        forWork
        withEnvrc
        withDco
        (withExtraFiles ./go-tuf)
      ];
      securesystemslib = pipe (gh "secure-systems-lab/securesystemslib") [
        myFork
        forWork
        withEnvrc
        (withExtraFiles ./securesystemslib)
        withDco
      ];
      nix-doom-emacs =
        pipe (gh "nix-community/nix-doom-emacs") [ myFork withEnvrc ];
    };
  };
}
