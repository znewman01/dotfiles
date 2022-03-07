{ config, pkgs, ... }:

{
  environment.etc."ssh/sshd_config.d/nopasswds".text = ''
    PasswordAuthentication no
    ChallengeResponseAuthentication no
  '';
}

