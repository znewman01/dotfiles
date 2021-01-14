{ config, lib, pkgs, ... }:

{
  fonts.fontconfig.enable = true;

  home.packages = with pkgs; [
    corefonts
    source-code-pro
    hack-font
    eb-garamond
    fira-code
    font-awesome-ttf
    iosevka
    liberation_ttf
    libertine
    libertinus
    noto-fonts
    noto-fonts-emoji
    noto-fonts-extra
    roboto-mono
    ttf_bitstream_vera
  ];
}
