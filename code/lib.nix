{ pkgs, lib }:

rec {
  forWork = lib.recursiveUpdate { config.user.email = "zjn@chainguard.dev"; };
  withEnvrc = lib.recursiveUpdate {
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
    in lib.recursiveUpdate { config.commit.template = "${gitTemplate}"; } p;
  withGitsign = p:
    lib.recursiveUpdate {
      config.commit.gpgsign = true;
      config.tag.gpgsign = true;
    } p;

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
  withExtraFiles = dir: lib.recursiveUpdate { extraFilesDir = dir; };
}
