with (import <nixpkgs> {});
derivation {
  name = "backup-scripts";
  builder = "${bash}/bin/bash";
  args = [ ./install.sh ];
  buildInputs = [ coreutils gnused ];
  inherit pass restic;
  system = builtins.currentSystem;
  src = [ ./backup.sh ./restic_common.sh ./restic.sh ];
}
