{ config, lib, pkgs, ... }:

with lib;

let
  forWork = recursiveUpdate { config.user.email = "zjn@chainguard.dev"; };
  withEnvrc = recursiveUpdate {
    extraFiles.".envrc".text = "use_flake";
    extraExcludes = [ ".direnv/" ];
  };
  gh = x: "git@github.com:${x}";
  myGH = name: gh "znewman01/${name}";

  withDco = p:
    let
      email = p.config.user.email or "z@znewman.net";
      gitTemplate = pkgs.writeText "git-commit-template" ''


        Signed-off-by: Zachary Newman <${email}>
      '';
    in recursiveUpdate { config.commit.template = "${gitTemplate}"; } p;

  # a code repo with my znewman01/ fork as origin and the original URL as upstream
  myFork = url: {
    url = let
      # git@github.com : <user or org> / repo
      # 0              1 2             3 4
      parts = (builtins.split "[:/]" url);
      repo = builtins.elemAt parts 4;
    in myGH repo;
    extraRemotes.upstream = url;
  };
  withExtraFiles = dir: recursiveUpdate { extraFilesDir = dir; };
in {
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
