{ config, lib, pkgs, ... }:

{
  fonts.fontconfig.enable = true;

  home.packages = with pkgs; [
    corefonts
    source-code-pro
    hack-font
    font-awesome-ttf
    liberation_ttf
    libertinus
    noto-fonts
    noto-fonts-emoji
    noto-fonts-extra
    ttf_bitstream_vera
  ];
}
