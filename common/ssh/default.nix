{ config, pkgs, lib, ... }:

{
  environment.etc."ssh/sshd_config.d/nopasswds" =
    lib.optionalAttrs pkgs.stdenv.isDarwin {
      text = ''
        PasswordAuthentication no
        ChallengeResponseAuthentication no
      '';
    };

  services = lib.optionalAttrs pkgs.stdenv.isLinux {
    openssh = {
      enable = true;
      hostKeys = [{
        path = "/persist/ssh/ssh_host_ed25519_key";
        type = "ed25519";
      }];
      settings.PermitRootLogin = "no";
      settings.PasswordAuthentication = false;
    };
  };
  users.users = lib.optionalAttrs pkgs.stdenv.isLinux {
    root.openssh.authorizedKeys.keyFiles = (import ../../net/keys.nix);
    zjn.openssh.authorizedKeys.keyFiles = (import ../../net/keys.nix);
  };

  # TODO: move to home.nix when rycee/home-manager#1087 resolved
  # https://github.com/rycee/home-manager/issues/1087
  programs.ssh = lib.optionalAttrs pkgs.stdenv.isLinux { startAgent = true; };
}
