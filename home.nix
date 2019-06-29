{ config, pkgs, ... }:

let
  home = "/home/zjn";
  dots = "${home}/dotfiles";
  bgColor = "#282A36";
  fgColor = "#F8F8F2";
in
{
  programs.home-manager.enable = true;

  xsession.enable = true;
  xsession.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    config = pkgs.writeText "xmonad.hs" ''
      import XMonad
      import XMonad.Layout.NoBorders
      import XMonad.Layout.Spacing
      import XMonad.Hooks.ManageDocks
      import XMonad.Hooks.DynamicLog

      myBorderSpacing = spacingRaw True (Border 4 4 4 4) True (Border 4 4 4 4) True

      main = xmonad =<< statusBar "xmobar" myPP toggleStrutsKey myConfig

      -- Command to launch the bar.
      myPP = xmobarPP
          { ppCurrent = xmobarColor "${fgColor}" ""
          , ppHidden = xmobarColor "#6272A4" ""
          , ppLayout = const ""
          , ppTitle = const "" }
      toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

      -- Main configuration, override the defaults to your liking.
      myConfig = defaultConfig
          { terminal = "alacritty"
          , borderWidth = 3
          , layoutHook = avoidStruts $ smartBorders $ myBorderSpacing $ layoutHook defaultConfig
          , manageHook=manageHook defaultConfig <+> manageDocks
          , startupHook = startup
          , normalBorderColor = "${bgColor}"
          , focusedBorderColor = "${fgColor}" }

      startup :: X ()
      startup = do
        spawn "xsetroot -solid '#44475A'"
    '';
  };
  home.file.".xmobarrc".source = "${dots}/xmobarrc";

  home.packages = with pkgs; [
     haskellPackages.xmobar
     font-awesome_4
     hack-font
  ];


  programs.alacritty = {
    enable = true;
    settings.font.size = 7;
    settings.font.family = [ "Hack" "FontAwesome" ];
    settings.colors = {
      primary = {
        background = "${bgColor}";
        foreground = "${fgColor}";
      };
      normal = {
        black = "0x000000";
        red = "0xff5555";
        green = "0x50fa7b";
        yellow = "0xf1fa8c";
        blue = "0xcaa9fa";
        magenta = "0xff79c6";
        cyan = "0x8be9fd";
        white = "0xbfbfbf";
      };
      bright = {
        black = "0x575b70";
        red = "0xff6e67";
        green = "0x5af78e";
        yellow = "0xf4f99d";
        blue = "0xcaa9fa";
        magenta = "0xff92d0";
        cyan = "0x9aedfe";
        white = "0xe6e6e6";
      };
    };
  };

  programs.git = {
    enable = true;
    ignores = [ "*~" "*.swp" ];
    userEmail = "z@znewman.net";
    userName = "Zachary Newman";
  };

  programs.firefox = {
    enable = true;
  };


  programs.bash = {
    enable = true;
    sessionVariables = {
      EDITOR = "vim";
    };
  };

  fonts.fontconfig.enable = true;
}
