{ config, pkgs, ... }:

{
  home.username = "zjn";

  home.enableNixpkgsReleaseCheck = true;

  programs.bash = {
    enable = true;
    sessionVariables = { PATH = "$HOME/bin:$PATH"; };
  };

  programs.home-manager.enable = true;
  programs.zsh.enable = pkgs.stdenv.isDarwin;

  imports = [ ./vim.nix ./git.nix ../modules/code.nix ./ssh/home.nix ];
}
