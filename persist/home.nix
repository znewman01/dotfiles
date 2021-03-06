{ config, pkgs, ... }:

let
  rev = "58558845bc68dcf2bb32caa80564f7fe3f6cbc61";
  impermanence = builtins.fetchTarball {
    # git ls-remote https://github.com/nix-community/impermanence master
    url = "https://github.com/nix-community/impermanence/archive/${rev}.tar.gz";
    # To update, make all-0 and run nixos-rebuild switch
    sha256 = "10z3g4knkvq838zbfq71pkfyl8cffrpavna448wf5mjscycp0gnv";
  };
in {
  imports = [ "${impermanence}/home-manager.nix" ];

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
      ".terraform.d"
      ".packer.d"
    ];
    files = [ ".gist" ];
    allowOther = true;
  };
}
