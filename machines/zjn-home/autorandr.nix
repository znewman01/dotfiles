{ config, pkgs, ... }:

let
  fingerprintDell24 = {
    "HDMI-A-1" =
      "00ffffffffffff0010acbca0555656321c1c010380342078ea0495a9554d9d26105054a54b00714f8180a940d1c0d100010101010101283c80a070b023403020360006442100001e000000ff00434656394e383744325656550a000000fc0044454c4c2055323431350a2020000000fd00313d1e5311000a2020202020200121020322f14f9005040302071601141f12132021222309070765030c00200083010000023a801871382d40582c450006442100001e011d8018711c1620582c250006442100009e011d007251d01e206e28550006442100001e8c0ad08a20e02d10103e960006442100001800000000000000000000000000000000000000000072";
  };
  fingerprintLg32 = {
    "HDMI-A-0" =
      "00ffffffffffff001e6d955bb76600000c1e010380462778ea9fd1a2574c9c240c5054256b807140818081c0a9c0b300d1c08100d1cf565e00a0a0a0295030203500ba882100001a000000fd00304b1e701f010a202020202020000000fc004c4720484452205148440a2020000000ff003031324e54565330533239350a011d020331f14b0103049012131f225d5e5f23090707830100006d030c001000b83e200060010203e305c000e6060501595928023a801871382d40582c4500ba882100001e011d007251d01e206e285500ba882100001e30750090a0a0285030203500ba882100001a0000000000000000000000ba00000000000000000000000004";
  };
in {
  programs.autorandr.profiles = {
    "home" = {
      fingerprint = fingerprintLg32 // fingerprintDell24;
      config = {
        "HDMI-A-0" = {
          position = "0x100";
          mode = "2560x1440";
        };
        "HDMI-A-1" = {
          position = "2560x0";
          mode = "1920x1200";
          rotate = "right";
        };
      };
    };
  };
}
