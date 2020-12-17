{ config, lib, pkgs, ... }:

with lib;

let

  dag = config.lib.dag;
  cfg = config.code;

  storeFileName = path:
    let
      # All characters that are considered safe. Note "-" is not
      # included to avoid "-" followed by digit being interpreted as a
      # version.
      safeChars =
        [ "+" "." "_" "?" "=" ]
        ++ lowerChars
        ++ upperChars
        ++ stringToCharacters "0123456789";

      empties = l: genList (x: "") (length l);

      unsafeInName = stringToCharacters (
        replaceStrings safeChars (empties safeChars) path
      );

      safeName = replaceStrings unsafeInName (empties unsafeInName) path;
    in
      "hm_" + safeName;

  # TODO: use home-manager/modules/lib/file-type.nix directly
  fileType = types.submodule (
    { name, config, ... }: {
      options = {
        target = mkOption {
          type = types.str;
          description = "Path to target file relative to repo root.";
        };

        text = mkOption {
          type = types.lines;
          description = "Text of the file.";
        };

        executable = mkOption {
          type = types.nullOr types.bool;
          default = null;
          description = ''
            Set the execute bit. If <literal>null</literal>, defaults to the mode
            of the <varname>source</varname> file or to <literal>false</literal>
            for files created through the <varname>text</varname> option.
          '';
        };

        source = mkOption {
          type = types.path;
          description = ''
            Path of the source file. The file name must not start
            with a period since Nix will not allow such names in
            the Nix store.
            </para><para>
            This may refer to a directory.
          '';
        };
      };

      config = {
        target = mkDefault name;
        source = mkIf (config.text != null) (
          mkDefault (pkgs.writeTextFile {
            inherit (config) executable text;
            name = storeFileName name;
          })
        );
      };
    }
  );

  excludeSubmodule = types.submodule {
    options = {

      enable = mkEnableOption ''
        Manage .git/info/exclude.

        If enabled, include any files that we drop in.
      '';

      text = mkOption {
        type = types.lines;
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

      extraFiles = mkOption {
        type = types.loaOf fileType;
        default = {};
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
          (removePrefix "${dirname}/")
          ((attrNames repo.extraFiles) ++ (attrNames shellNixFiles) ++ [".direnv"]);
        excludeFiles = optionalAttrs (repo.exclude.enable) {
          "${dirname}/.git/info/exclude".text = concatStringsSep "\n\n" [
            ourFiles
            repo.exclude.text
          ];
        };
        moveToRepoDir = (name: value:
          nameValuePair
            "${dirname}/${name}"
            (value // { "target" = "${dirname}/${name}"; })
        );
        extraFiles = mapAttrs' moveToRepoDir repo.extraFiles;
      in mkMerge [ shellNixFiles excludeFiles extraFiles ]
    );

  in

    mkIf (cfg.baseDir != null) {
      home.activation.cloneRepos =
        dag.entryBetween [ "linkGeneration" ] [ "writeBoundary" ] ''
          mkdir -p ${cfg.baseDir}
          ${concatStringsSep "\n" (mapAttrsToList cloneRepoSh cfg.repos)}
        '';

      home.file = mkMerge (mapAttrsToList additionalFiles cfg.repos);
    };

}
