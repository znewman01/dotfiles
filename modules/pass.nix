{ config, pkgs, lib, ... }:

with lib;

let cfg = config.programs.pass;
in {
  options.programs.pass = {
    enable = mkOption {
      type = types.bool;
      description = "Whether to enable pass.";
      default = false;
    };
    store = mkOption {
      type = types.str;
      description = "Directories to look ifor the store";
      example = "$HOME/.password-store";
    };
  };

  config = mkIf cfg.enable {
    home.packages = let
      wrappedPass = pkgs.symlinkJoin {
        name = "pass";
        paths = [ pkgs.pass ];
        buildInputs = [ pkgs.makeWrapper ];
        postBuild = ''
          wrapProgram $out/bin/pass \
            --set-default PASSWORD_STORE_DIR ${cfg.store}
          mv $out/share/bash-completion/completions/pass{,.bak}
          echo 'export "PASSWORD_STORE_DIR=''${PASSWORD_STORE_DIR-${cfg.store}}"' > $out/share/bash-completion/completions/pass
          cat $out/share/bash-completion/completions/pass.bak >> $out/share/bash-completion/completions/pass
          rm $out/share/bash-completion/completions/pass.bak
        '';
      };
    in [ wrappedPass ];
    home.file.".authinfo.gpg".source =
      config.lib.file.mkOutOfStoreSymlink "${cfg.store}/authinfo.gpg";
  };
}
