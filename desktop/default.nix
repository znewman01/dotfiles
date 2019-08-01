{ config, lib, pkgs, ... }:

{
  imports = [
    ./xmonad.nix
    ./fonts.nix
    ./alacritty.nix
  ];

  home.packages = with pkgs; [
    anki
    i3lock
    xautolock
    xclip
    xdotool
    zathura
  ];

  services.screen-locker = {
    enable = true;
    inactiveInterval = 10;
    lockCmd = "${pkgs.i3lock}/bin/i3lock -n";
  };

  programs.autorandr = {
    enable = true;
    profiles = {
      "workstation" = {
        fingerprint = {
          "HDMI-4" = "00ffffffffffff0010acb8a0555656321c1c0104a53420783a0495a9554d9d26105054a54b00714f8180a940d1c0d100010101010101283c80a070b023403020360006442100001e000000ff00434656394e383744325656550a000000fc0044454c4c2055323431350a2020000000fd00313d1e5311000a20202020202001af02031cf14f9005040302071601141f12132021222309070783010000023a801871382d40582c450006442100001e011d8018711c1620582c250006442100009e011d007251d01e206e28550006442100001e8c0ad08a20e02d10103e96000644210000180000000000000000000000000000000000000000000000000000000c";
          "HDMI-5" = "00ffffffffffff0010ac5b404c3843430c16010380261e78eeee95a3544c99260f5054a54b00714f8180010101010101010101010101302a009851002a4030701300782d1100001e000000ff004d33394d4432334e4343384c0a000000fc0044454c4c2050313930530a2020000000fd00384c1e510e000a2020202020200090";
        };
        config = {
          "DP-3".enable = false;
          "HDMI-3".enable = false;
          "HDMI-4" = {
            enable = true;
            primary = true;
            position = "1024x0";
            gamma = "1.0:0.909:0.833";
            rate = "59.95";
            mode = "1920x1200";
          };
          "HDMI-5" = {
            enable = true;
            position = "0x0";
            rotate = "right";
            gamma = "1.0:0.909:0.833";
            rate = "60.02";
            mode = "1280x1024";
          };
        };
      };
      "laptop-docked" = {
        fingerprint = {
          "DP-1" = "00ffffffffffff0010acb8a0555656321c1c0104a53420783a0495a9554d9d26105054a54b00714f8180a940d1c0d100010101010101283c80a070b023403020360006442100001e000000ff00434656394e383744325656550a000000fc0044454c4c2055323431350a2020000000fd00313d1e5311000a20202020202001af02031cf14f9005040302071601141f12132021222309070783010000023a801871382d40582c450006442100001e011d8018711c1620582c250006442100009e011d007251d01e206e28550006442100001e8c0ad08a20e02d10103e96000644210000180000000000000000000000000000000000000000000000000000000c";
          "LVDS-1" = "00ffffffffffff0030e4d3020000000000150103801c1078ea10a59658578f2820505400000001010101010101010101010101010101381d56d45000163030202500159c1000001b000000000000000000000000000000000000000000fe004c4720446973706c61790a2020000000fe004c503132355748322d544c423100f7";
        };
        config = {
          "VGA-1".enable = false;
          "HDMI-1".enable = false;
          "HDMI-2".enable = false;
          "HDMI-3".enable = false;
          "DP-1" = {
            enable = true;
            gamma = "1.0:0.909:0.833";
            mode = "1920x1200";
            position = "0x0";
            rate = "59.95";
          };
          "DP-2".enable = false;
          "DP-3".enable = false;
          "LVDS-1" = {
            enable = true;
            primary = true;
            gamma = "1.0:0.909:0.833";
            mode = "1366x768";
            position = "1920x0";
            rate = "60.00";
          };
        };
      };
      "laptop-undocked" = {
        fingerprint = {
          "LVDS-1" = "00ffffffffffff0030e4d3020000000000150103801c1078ea10a59658578f2820505400000001010101010101010101010101010101381d56d45000163030202500159c1000001b000000000000000000000000000000000000000000fe004c4720446973706c61790a2020000000fe004c503132355748322d544c423100f7";
        };
        config = {
          "VGA-1".enable = false;
          "HDMI-1".enable = false;
          "HDMI-2".enable = false;
          "HDMI-3".enable = false;
          "DP-1".enable = false;
          "DP-2".enable = false;
          "DP-3".enable = false;
          "LVDS-1" = {
            enable = true;
            primary = true;
            gamma = "1.0:0.909:0.833";
            mode = "1366x768";
            position = "0x0";
            rate = "60.00";
          };
        };
      };
    };
  };
}
