with (import <nixpkgs> {});
derivation {
  name = "backup-scripts";
  builder = "${bash}/bin/bash";
  args = [ ./install.sh ];
  buildInputs = [ coreutils gnused ];
  inherit pass restic jq;
  system = builtins.currentSystem;
  src = [
    ./backup.sh
    ./enforce_backup_recency.sh
    ./restic_common.sh
    ./restic.sh
    ./prune_backups.sh
  ];
}
