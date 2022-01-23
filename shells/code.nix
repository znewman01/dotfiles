{ config, ... }:

{
  baseDir = "${config.home.homeDirectory}/git";
  repos = let
    blackFiles = {
      ".dir-locals.el".text = ''
        ((python-mode . ((blacken-mode . t))))
      '';
    };
  in {
    "iacr-dl" = {
      url = "git@github.com:znewman01/iacr-dl.git";
      shell = ./iacr.nix;
      exclude.enable = true;
      extraFiles = blackFiles;
    };
    "resume" = {
      url = "git@github.com:znewman01/resume.git";
      exclude.enable = true;
    };
    "spectrum-paper" = {
      url = "git@github.com:sachaservan/spectrum-paper.git";
      exclude.enable = true;
      shell = ./spectrum-paper.nix;
    };
    "spectrum-impl" = {
      url = "git@github.com:znewman01/spectrum-impl.git";
      exclude.enable = true;
    };
    "dotfiles" = { url = "git@github.com:znewman01/dotfiles.git"; };
    "scalingsnapshots" = {
      url = "git@github.com:znewman01/scalingsnapshots.git";
      exclude.enable = true;
      extraFiles = {
        "analysis/.projectile".text = "";
        ".envrc".text = ''
          use_nix
          export GOOGLE_APPLICATION_CREDENTIALS="$HOME/Sync/keys/gcp-scalingsnapshots.json"
        '';
      };
    };
  };
}
