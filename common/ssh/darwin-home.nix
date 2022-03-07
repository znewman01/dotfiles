{ config, lib, pkgs, ... }:

{
  imports = [ ./home.nix ];

  home.file.".ssh/authorized_keys.src" = {
    text = lib.concatStrings (map builtins.readFile (import ../../net/keys.nix));
    # Hack: sshd doesn't like the permissions for a symlink to /nix/store
    onChange = "cat ~/.ssh/authorized_keys.src > ~/.ssh/authorized_keys";
  };
}
