{ config, pkgs, ... }:

{
  home.enableNixpkgsReleaseCheck = true;

  programs.bash = {
    enable = true;
    sessionVariables = { PATH = "$HOME/bin:$PATH"; };
  };

  programs.home-manager.enable = true;

  imports = [ ./vim.nix ./git.nix ../modules/code.nix ];
}
