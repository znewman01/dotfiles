{ config, pkgs, ... }:

{
  programs.git = {
    enable = true;
    ignores = [ "*~" "*.swp" ];
    userEmail = "z@znewman.net";
    userName = "Zachary Newman";
    extraConfig.pull.rebase = true;
    extraConfig.github.user = "znewman01";
    extraConfig.init.defaultBranch = "main";
    extraConfig.push.autoSetupRemote = true;
    extraConfig.commit.gpgsign = true;
    extraConfig.tag.gpgsign = true;
    extraConfig.gpg.x509.program = "gitsign";
    extraConfig.gpg.format = "x509";
  };
}
