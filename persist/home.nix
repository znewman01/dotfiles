{ config, pkgs, ... }:

{
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
      ".config/Element"
    ];
    allowOther = true;
  };

  # Things that are annoying to reconfigure/rebuild/redownload but not a real
  # problem to lose.
  home.persistence."/cache/zjn" = {
    directories = [
      ".cache/"
      ".mozilla"
      ".local/doom"
      ".local/share/Anki"
      ".local/share/Anki2"
      ".local/share/direnv"
      ".local/share/keyrings"
      ".config/skypeforlinux"
      ".config/Slack"
      ".config/VSCodium"
      ".vagrant.d"
      ".terraform.d"
    ];
    files = [ ".gist" ];
    allowOther = true;
  };
}
