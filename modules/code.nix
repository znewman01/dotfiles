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
        type = types.attrsOf types.str;
        default = { };
      };

      extraFiles = mkOption {
        type = types.attrsOf types.anything;
        default = { };
      };

      extraFilesDir = mkOption {
        type = types.nullOr types.path;
        default = null;
        description = "Path to directory containing extra files.";
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
        # TODO: should probably make sure remotes etc. are correct
      in ''
        if [ ! -d "${dirname}" ]; then
          mkdir -p "${dirname}"
          ${pkgs.git}/bin/git init "${dirname}"
        fi
      '';

    additionalFiles = name: repo:
      let
        dirname = getDirname name repo;
        configFile = let
          filesToExclude = repo.extraExcludes ++ (attrNames repo.extraFiles);
          excludeFile = pkgs.writeText "exclude"
            ((concatStringsSep "\n" filesToExclude) + "\n");
          makeRemote = name: url:
            nameValuePair ''remote "${name}"'' {
              inherit url;
              fetch = "+refs/heads/*:refs/remotes/${name}/*";
            };
          allRemotes = { "origin" = repo.url; } // repo.extraRemotes;
          remoteConfig = listToAttrs (mapAttrsToList makeRemote allRemotes);
          gitconfig = repo.config // remoteConfig
            // optionalAttrs (excludeFile != { }) {
              core.excludesFile = excludeFile;
            };
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
        };
        # TODO: symlinking doesn't work well at all for flakes. you have to copy
        # them in. That's going to be a pain.
        extraFilesFromDir = optionalAttrs (repo.extraFilesDir != null)
          (listToAttrs (map (file:
            nameValuePair "${dirname}/${file}" {
              source = repo.extraFilesDir + "/${file}";
            }) (attrNames (builtins.readDir repo.extraFilesDir))));
        extraFilesFromConfig =
          mapAttrs' (key: value: nameValuePair "${dirname}/${key}" value)
          repo.extraFiles;
      in configFile // extraFilesFromDir // extraFilesFromConfig;

  in mkIf (cfg.baseDir != null) {
    home.activation.cloneRepos =
      dag.entryBetween [ "linkGeneration" ] [ "writeBoundary" ] ''
        mkdir -p ${cfg.baseDir}
        ${concatStringsSep "\n" (mapAttrsToList configureRepoSh cfg.repos)}
      '';

    home.file = mkMerge (mapAttrsToList additionalFiles cfg.repos);
  };

}
