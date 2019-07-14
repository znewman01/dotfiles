{ config, lib, pkgs, ... }:

with lib;

let

  dag = config.lib.dag;
  cfg = config.code;

  excludeSubmodule = types.submodule {
    options = {

      enable = mkEnableOption ''
        Manage .git/info/exclude.

        If enabled, include any files that we drop in.
      '';

      text = mkOption {
        type = types.string;
        example = ''
          .envrc
          shell.nix
        '';
        default = "";
        description = "TODO";
      };

    };
  };

  repoSubmodule = types.submodule {
    options = {

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

      exclude = mkOption {
        type = types.nullOr excludeSubmodule;
        default = { enable = false; };
        # TODO: example
        description = "TODO";
      };

      # TODO: more git config?
    };
  };

in

{

  options = {
    code = {
      baseDir = mkOption {
        type = with types; nullOr string;
        example = "$HOME/code";
        default = null;
        description = ''
          Default directory in which given repos will be cloned.

          Must be given to use this feature.
        '';
      };


      repos = mkOption {
        type = with types; attrsOf repoSubmodule;
        example = {
          "tensorflow" = {
          };
        };
        default = {};
        description = "Repos to clone.";
      };
    };
  };

  config = let

    getDirname = name: repo: "${cfg.baseDir}/${name}";

    cloneRepoSh = name: repo: (
      let
        dirname = getDirname name repo;
        manageExcludes = if repo.exclude.enable then "1" else "0";
        # TODO: should probably make sure remotes etc. are correct
      in ''
        if [ ! -d "${dirname}" ]; then
          $DRY_RUN_CMD ${pkgs.git}/bin/git clone ${repo.url} "${dirname}"
          if [ "${manageExcludes}" -eq 1 ]; then
            $DRY_RUN_CMD rm "${dirname}/.git/info/exclude"
          fi
        fi
      ''
    );

    additionalFiles = name: repo: (
      let
        dirname = getDirname name repo;
        shell = repo.shell;
        envrc = "${dirname}/.envrc";
        shellNixFiles = optionalAttrs (repo.shell != null) {
          "${dirname}/shell.nix".source = repo.shell;
          "${envrc}" = {
            text = "use_nix";
            onChange = "$DRY_RUN_CMD ${pkgs.direnv}/bin/direnv allow ${envrc}";
          };
        };
        ourFiles = concatMapStringsSep "\n"
          (removePrefix dirname)
          (attrNames shellNixFiles);
        excludeFiles = optionalAttrs (repo.exclude.enable) {
          "${dirname}/.git/info/exclude".text = concatStringsSep "\n\n" [
            ourFiles
            repo.exclude.text
          ];
        };
      in mkMerge [ shellNixFiles excludeFiles ]
    );

  in

    {
      home.activation.cloneRepos =
        dag.entryBetween [ "linkGeneration" ] [ "writeBoundary" ] ''
          mkdir -p ${cfg.baseDir}
          ${concatStringsSep "\n" (mapAttrsToList cloneRepoSh cfg.repos)}
        '';

      home.file = mkMerge (mapAttrsToList additionalFiles cfg.repos);
    };

}
