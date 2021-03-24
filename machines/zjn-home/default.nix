{ config, pkgs, ... }:

let
  impermanence = builtins.fetchTarball {
    url =
      "https://github.com/nix-community/impermanence/archive/master.tar.gz";
  };
in {
  imports = [ "${impermanence}/home-manager.nix" ];

  # Things that should get real backups.
  # Heuristic: are the source of truth and important.
  # So: keys, cloud-based things where local changes might need to be recovered
  # (Dropbox/Syncthing/code)
  home.persistence."/persist/zjn" = {
    directories = [ "git" ".gnupg" ".dropbox-hm" ".ssh" ".emacs.d" "Sync" ];
    files = [ ".gist" ];
    allowOther = true;
  };

  # Things that are annoying to reconfigure/rebuild/redownload but not a real
  # problem to lose.
  home.persistence."/cache/zjn" = {
    directories = [ ".cache/mu" "Maildir" ".emacs.d.template" ".mozilla" ".config/syncthing" ".local/share/direnv" ];
    allowOther = true;
  };
}