{ config, pkgs, ... }:

let
  impermanence = builtins.fetchTarball {
    url = "https://github.com/nix-community/impermanence/archive/master.tar.gz";
  };
in {
  imports = [ "${impermanence}/home-manager.nix" ./autorandr.nix ];

  # Things that should get real backups.
  # Heuristic: are the source of truth and important.
  # So: keys, cloud-based things where local changes might need to be recovered
  # (Syncthing/code)
  home.persistence."/persist/zjn" = {
    directories = [
      "git"
      ".gnupg"
      ".ssh"
      "Sync"
      ".config/keybase"
      ".config/Signal"
      ".config/syncthing"
    ];
    allowOther = true;
  };

  # Things that are annoying to reconfigure/rebuild/redownload but not a real
  # problem to lose.
  home.persistence."/cache/zjn" = {
    directories = [
      ".cache/mu"
      ".cache/Tectonic"
      "Maildir"
      ".emacs.d"
      ".emacs.d.template"
      ".mozilla"
      ".local/share/direnv"
      ".local/share/keyrings"
      ".local/share/tridactyl"
      ".config/skypeforlinux"
      ".config/Slack"
      ".vagrant.d"
    ];
    files = [ ".gist" ];
    allowOther = true;
  };
}
