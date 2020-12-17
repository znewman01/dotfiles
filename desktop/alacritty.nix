{ config, lib, pkgs, ... }:

let
  # TODO: make module
  fuiTurquoise = "0x1abc9c";
  fuiEmerald = "0x2ecc71";
  fuiRiver = "0x3498db";
  fuiAmethyst = "0x9b59b6";
  fuiDeepAsphalt = "0x34495e";
  fuiAsphalt = "0x425d78";
  fuiSunflower = "0xf1c40f";
  fuiCarrot = "0xe67e22";
  fuiAlizarin = "0xe74c3c";
  fuiClouds = "0xecf0f1";
  fuiConcrete = "0x95a5a6";
  fuiDarkTurquoise = "0x16a085";
  fuiDarkEmerald = "0x27ae60";
  fuiDarkRiver = "0x2980b9";
  fuiDarkAmethyst = "0x8e44ad";
  fuiDarkAsphalt = "0x2c3e50";
  fuiDarkSunflower = "0xf39c12";
  fuiDarkCarrot = "0xd35400";
  fuiDarkAlizarin = "0xc0392b";
  fuiDarkClouds = "0xbdc3c7";
  fuiDeepClouds = "0xdce0e1";
  fuiDarkConcrete = "0x7f8c8d";
  bgColorLight = fuiClouds;
  fgColorLight = fuiAsphalt;
  bgColorDark = fuiDarkAsphalt;
  fgColorDark = fuiDarkClouds;
in
{
  programs.alacritty = {
    enable = true;
    settings.font = {
      size = 6;
      family = [ "Iosevka" "Font Awesome 5 Brands" "Font Awesome 5 Free" "Font Awesome 5 Free Solid" ];
      offset.y = 2;
    };
    settings.window.padding = {
      x = 8;
      y = 0;
    };
    settings.colors = {
      primary = {
        background = "${bgColorLight}";
        foreground = "${fgColorLight}";
      };
      normal = {
        black = "0x000000";
        red = "${fuiDarkAlizarin}";
        green = "${fuiDarkEmerald}";
        yellow = "${fuiDarkSunflower}";
        blue = "${fuiDarkTurquoise}";
        magenta = "${fuiDarkAmethyst}";
        cyan = "${fuiDarkRiver}";
        white = "${fuiDarkClouds}";
      };
      bright = {
        black = "0x000000";
        red = "${fuiAlizarin}";
        green = "${fuiEmerald}";
        yellow = "${fuiSunflower}";
        blue = "${fuiTurquoise}";
        magenta = "${fuiAmethyst}";
        cyan = "${fuiRiver}";
        white = "${fuiClouds}";
      };
    };
  };
}
