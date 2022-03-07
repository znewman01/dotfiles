{ config, pkgs, ... }:

{
  imports = [
    ./vim.nix
    ./git.nix
  ];
}

