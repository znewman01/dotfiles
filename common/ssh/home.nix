{ config, pkgs, lib, ... }:

{
  home.packages = [ pkgs.openssh ];
  programs.ssh.enable = true;

  programs.ssh.matchBlocks = lib.optionalAttrs pkgs.stdenv.isDarwin {
    "*" = {
      extraOptions = {
        IdentityAgent = "/Users/zjn/Library/Containers/com.maxgoedjen.Secretive.SecretAgent/Data/socket.ssh";
      };
    };
    # Use SK for GitHub
    "github.com" = {
      hostname = "github.com";
      user = "git";
      # identityFile = "~/.ssh/id_ed25519_sk";
    };
  };

  home.file = lib.optionalAttrs pkgs.stdenv.isDarwin {
    ".ssh/authorized_keys.src" = {
      text =
        lib.concatStrings (map builtins.readFile (import ../../net/keys.nix));
      # Hack: sshd doesn't like the permissions for a symlink to /nix/store
      onChange = "cat ~/.ssh/authorized_keys.src > ~/.ssh/authorized_keys";
    };
  };
}
