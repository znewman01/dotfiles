{ config, pkgs, ... }:

{
   home.file.".ssh/authorized_keys.src" = {
     text = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBA+fsi2MONdZ65XIrD+e5EYfPqcZrG4Fd0E4VMz9YHQ zjn@zjn-work";
     # Hack: sshd doesn't like the permissions for a symlink to /nix/store
     onChange = "cat ~/.ssh/authorized_keys.src > ~/.ssh/authorized_keys";
  };
}
