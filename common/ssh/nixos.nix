{ config, pkgs, ... }:

{
  services.openssh = {
    enable = true;
    hostKeys = [{
      path = "/persist/ssh/ssh_host_ed25519_key";
      type = "ed25519";
    }];
    settings.PermitRootLogin = "no";
    settings.PasswordAuthentication = false;
  };
  users.users.root.openssh.authorizedKeys.keyFiles = (import ../../net/keys.nix);
  users.users.zjn.openssh.authorizedKeys.keyFiles = (import ../../net/keys.nix);

  # TODO: move to home.nix when rycee/home-manager#1087 resolved
  # https://github.com/rycee/home-manager/issues/1087
  programs.ssh.startAgent = true;
}

