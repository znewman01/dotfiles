{ config, lib, pkgs, ... }:

with lib;

let

  dag = config.lib.dag;
  # file-type = config.lib.file-type;
  cfg = config.code;

  repoSubmodule = types.submodule {
    options = {
      url = mkOption {
        type = types.str;
        example = "git@github.com:tensorflow/tensorflow.git";
        description = "The Git URL to use for the origin remote.";
      };

      extraRemotes = mkOption {
        type = types.attrsOf types.str;
        default = { };
        example = { "upstream" = "git@github.com:user/repo"; };
      };

      extraExcludes = mkOption {
        type = types.listOf types.str;
        default = [ ];
      };

      config = mkOption {
        type = types.attrsOf types.anything;
        default = { };
      };

      extraFiles = mkOption {
        type = types.attrsOf types.anything;
        default = { };
      };

      extraFilesDir = mkOption {
        type = types.nullOr types.path;
        default = null;
        description = ''
          Path to directory containing extra files.

          These will be *copied* (not linked) into the cloned repository
          directory and added to the excludes file.
        '';
      };
    };
  };

in {

  options = {
    code = {
      baseDir = mkOption {
        type = with types; nullOr str;
        example = "$HOME/code";
        default = null;
        description = ''
          Default directory in which given repos will be cloned.

          Must be given to use this feature.
        '';
      };

      repos = mkOption {
        type = with types; attrsOf repoSubmodule;
        example = { "tensorflow" = { }; };
        default = { };
        description = "Repos to clone.";
      };
    };
  };

  config = let

    getDirname = name: repo: "${cfg.baseDir}/${name}";

    configureRepoSh = name: repo:
      let
        dirname = getDirname name repo;
        # symlinking doesn't work well at all for flakes. you have to copy
        # them in.
        maybeCopyExtraFilesFromDir =
          optionalString (repo.extraFilesDir != null) ''
            ${pkgs.rsync}/bin/rsync --perms --recursive --chmod=u+w ${repo.extraFilesDir}/ ${dirname}/
          '';
      in ''
        if [ ! -d "${dirname}" ]; then
          mkdir -p "${dirname}"
          ${pkgs.git}/bin/git init "${dirname}"
        fi
        # TODO: should probably make sure remotes etc. are correct if directory
        # already exists
        ${maybeCopyExtraFilesFromDir}
      '';

    additionalFiles = name: repo:
      let
        dirname = getDirname name repo;
        configFile = let
          filesToExclude = repo.extraExcludes ++ (attrNames repo.extraFiles)
            ++ (optionals (repo.extraFilesDir != null)
            # Add files from extraFilesDir to git excludes
              (map (removePrefix (builtins.toString repo.extraFilesDir))
                (map builtins.toString
                  (filesystem.listFilesRecursive repo.extraFilesDir))));
          makeRemote = name: url:
            nameValuePair ''remote "${name}"'' {
              inherit url;
              fetch = "+refs/heads/*:refs/remotes/${name}/*";
            };
          allRemotes =
            recursiveUpdate { "origin" = repo.url; } repo.extraRemotes;
          remoteConfig = listToAttrs (mapAttrsToList makeRemote allRemotes);
          gitconfig = foldr recursiveUpdate { } [ repo.config remoteConfig ];
        in {
          "${dirname}/.git/config-nix" = {
            text = generators.toINI { } gitconfig;
            onChange = ''
              GITCONFIG="${dirname}/.git/config"
              grep -q "path = config-nix" $GITCONFIG \
                || echo -e "[include]\n  path = config-nix" \
                >> $GITCONFIG
            '';
          };
          "${dirname}/.git/info/exclude-nix" = {
            text = (concatStringsSep "\n" filesToExclude) + "\n";
            onChange = ''
              cat "${dirname}/.git/info/exclude-nix" > "${dirname}/.git/info/exclude"
            '';
          };
        };
        extraFilesFromConfig =
          mapAttrs' (key: value: nameValuePair "${dirname}/${key}" value)
          repo.extraFiles;
      in recursiveUpdate configFile extraFilesFromConfig;

  in mkIf (cfg.baseDir != null) {
    home.activation.cloneRepos = dag.entryAfter [ "writeBoundary" ] ''
      mkdir -p ${cfg.baseDir}
      ${concatStringsSep "\n" (mapAttrsToList configureRepoSh cfg.repos)}
    '';

    home.file = mkMerge (mapAttrsToList additionalFiles cfg.repos);
  };

}
