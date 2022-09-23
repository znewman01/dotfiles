{ config, ... }:

{
  code = {
    baseDir = "${config.home.homeDirectory}/git";
    repos = {
      "resume".url = "git@github.com:znewman01/resume.git";
      "dotfiles".url = "git@github.com:znewman01/dotfiles.git";
      "scalingsnapshots" = {
        url = "git@github.com:znewman01/scalingsnapshots.git";
        extraFiles = { "analysis/.projectile".text = ""; };
      };
      fulcio.url = "git@github.com:sigstore/fulcio.git";
      rekor.url = "git@github.com:sigstore/rekor.git";
      sigstore.url = "git@github.com:sigstore/sigstore.git";
      cosign.url = "git@github.com:sigstore/cosign.git";
      go-tuf = {
        url = "git@github.com:znewman01/go-tuf";
        extraRemotes.upstream = "git@github.com:theupdateframework/go-tuf";
        extraFilesDir = ./go-tuf;
      };
    };
  };
}
