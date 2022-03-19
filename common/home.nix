{ config, pkgs, ... }:

{
  home.enableNixpkgsReleaseCheck = true;
  nixpkgs.config.allowUnfree = true;

  programs.bash = {
    enable = true;
    sessionVariables = { PATH = "$HOME/bin:$PATH"; };
  };

  imports = [
    ./vim.nix
    ./git.nix
  ];
}

