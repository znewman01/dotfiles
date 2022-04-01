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
      extraFiles = { "analysis/.projectile".text = ""; };
    };
    "fulcio" = { url = "git@github.com:sigstore/fulcio.git"; };
    "rekor" = { url = "git@github.com:sigstore/rekor.git"; };
    "sigstore" = { url = "git@github.com:sigstore/sigstore.git"; };
    "cosign" = { url = "git@github.com:sigstore/cosign.git"; };
    "go-tuf" = { url = "git@github.com:theupdateframework/go-tuf.git"; };
  } // (import ./work.nix);
}
