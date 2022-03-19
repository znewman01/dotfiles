{ config, pkgs, ... }:

{
    imports = [
      ../emacs/nixos.nix
      ./gpg/nixos-home.nix
      ./web/firefox/nixos.nix
      ./home.nix
      ../persist/home.nix
      ../modules/code.nix
      ./xmonad.nix
      ./fonts.nix
      ./alacritty.nix
      ./screenlock-nixos-home.nix
      ./notifications-nixos-home.nix
      ./theme-nixos-home.nix
    ];

  home.packages = with pkgs; [
    pavucontrol
    xclip
    xdotool
    pinentry-gtk2
    gist
    xcompmgr
  ];

  xsession = {
    profileExtra = ''
      xcompmgr &
      autorandr -c &
    '';
    scriptPath = ".hm-xsession";
  };

  xdg.enable = true;

  home.keyboard.options = [ "caps:swapescape" "compose:ralt" ];

  programs.vscode = {
    enable = true;
    package = pkgs.vscodium;
    extensions = with pkgs.vscode-extensions; [
      vscodevim.vim
      golang.go
      eamodio.gitlens
    ];
  };

  programs.bash.shellAliases = {
    s = "systemctl";
    j = "journalctl";
  };

  programs.autorandr.enable = true;
}
