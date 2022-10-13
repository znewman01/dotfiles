{ config, lib, pkgs, ... }:

let
  forWork = p: { config.user.email = "zjn@chainguard.dev"; } // p;
  withEnvrc = p:
    {
      extraFiles."envrc".text = "use_flake";
      extraExcludes = [ ".direnv/" ];
    } // p;
  gh = x: "git@github.com:${x}";
  myGH = name: gh "znewman01/${name}";

  withDco = p:
    let
      email = p.config.user.email || "z@znewman.net";
      gitTemplate = pkgs.writeText ''


        Signed-off-by: Zachary Newman <${email}>
      '';
    in { config.commit.template = "${gitTemplate}"; } // p;

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
  withExtraFiles = dir: (p: { extraFilesDir = dir; } // p);
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
      fulcio = lib.pipe (gh "sigstore/fulcio") [ myFork forWork withDco ];
      rekor = lib.pipe (gh "sigstore/rekor") [ myFork forWork withDco ];
      sigstore = lib.pipe (gh "sigstore/sigstore") [ myFork forWork withDco ];
      cosign = lib.pipe (gh "sigstore/cosign") [ myFork forWork withDco ];
      go-tuf = lib.pipe (gh "theupdateframework/go-tuf") [
        myFork
        forWork
        withEnvrc
        withDco
        (withExtraFiles ./go-tuf)
      ];
      securesystemslib = lib.pipe (gh "secure-systems-lab/securesystemslib") [
        myFork
        forWork
        withEnvrc
        withDco
        (withExtraFiles ./securesystemslib)
      ];
      nix-doom-emacs =
        lib.pipe (gh "nix-community/nix-doom-emacs") [ myFork withEnvrc ];
    };
  };
}
