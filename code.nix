{ config, lib, pkgs, ... }:

with lib;

let

  dag = config.lib.dag;
  cfg = config.code;

  repoSubmodule = types.submodule {
    options = {

      name = mkOption {
        type = types.str;
        example = "tensorflow";
        description = "The name of the checked-out repo.";
      };

      url = mkOption {
        type = types.str;
        example = "git@github.com:tensorflow/tensorflow.git";
        description = "The Git URL to use for the remote.";
      };

      shell = mkOption {
        type = types.nullOr types.path;
        example = ./foo.nix;
        default = null;
        description = ''
          A .nix file to use as the `shell.nix` in this repository.

          Intended use case: repositories where you're the only Nix user. If
          provided, will link in the file in and populate a .envrc file in the
          repository root.
        '';
      };

    };
  };

in

{

  options = {
    code = {
      enable = mkEnableOption "Manage code directories.";

      baseDir = mkOption {
        type = types.string;
        example = "$HOME/git";
        description = "Default directory in which given repos will be cloned.";
      };


      repos = mkOption {
        type = types.listOf repoSubmodule;
        example = [
          {
          }
        ];
        default = [];
        description = "Repos to clone.";
      };
    };
  };

  config = let

    getDirname = repo: "${cfg.baseDir}/${repo.name}";

    cloneRepoSh = repo: (
      let
        dirname = getDirname repo;
        shell = optional repo.shell repo.shell;
        # TODO: should probably make sure remotes etc. are correct
      in ''
        if [ ! -d "${dirname}" ]; then
          $DRY_RUN_CMD ${pkgs.git}/bin/git clone ${repo.url} "${dirname}"
        fi
      ''
    );

    additionalFiles = repo: (
      let
        dirname = getDirname repo;
        shell = repo.shell;
        envrc = "${dirname}/.envrc";
      in optionalAttrs (repo.shell != null) {
        "${dirname}/shell.nix".source = repo.shell;
        "${envrc}" = {
          text = "use_nix";
          onChange = "$DRY_RUN_CMD ${pkgs.direnv}/bin/direnv allow ${envrc}";
        };
        # TODO: more git config. .git/info/exclude?
      }
    );

  in

    mkIf cfg.enable {

      home.activation.cloneRepos =
        dag.entryBetween [ "linkGeneration" ] [ "writeBoundary" ] ''
          mkdir -p ${cfg.baseDir}
          ${concatMapStringsSep "\n" cloneRepoSh cfg.repos}
          chmod -w ${cfg.baseDir}
        '';

      home.file = mkMerge (map additionalFiles cfg.repos);
    };

}
