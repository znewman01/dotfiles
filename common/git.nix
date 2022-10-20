{ config, pkgs, lib, ... }:

{
  programs.git = {
    enable = true;
    ignores = [ "*~" "*.swp" ]
      ++ lib.optionals pkgs.stdenv.isDarwin [ ".DS_Store" ];
    userEmail = "z@znewman.net";
    userName = "Zachary Newman";
    extraConfig.pull.rebase = true;
    extraConfig.github.user = "znewman01";
    extraConfig.init.defaultBranch = "main";
    extraConfig.push.autoSetupRemote = true;
    extraConfig.gpg.x509.program = "${pkgs.gitsign}/bin/gitsign";
    extraConfig.gpg.format = "x509";
  };
}
