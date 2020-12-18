{ config, pkgs, ... }:

let
  # TODO: make module
  fuiTurquoise = "#1abc9c";
  fuiEmerald = "#2ecc71";
  fuiRiver = "#3498db";
  fuiAmethyst = "#9b59b6";
  fuiDeepAsphalt = "#34495e";
  fuiAsphalt = "#425d78";
  fuiSunflower = "#f1c40f";
  fuiCarrot = "#e67e22";
  fuiAlizarin = "#e74c3c";
  fuiClouds = "#ecf0f1";
  fuiConcrete = "#95a5a6";
  fuiDarkTurquoise = "#16a085";
  fuiDarkEmerald = "#27ae60";
  fuiDarkRiver = "#2980b9";
  fuiDarkAmethyst = "#8e44ad";
  fuiDarkAsphalt = "#2c3e50";
  fuiDarkSunflower = "#f39c12";
  fuiDarkCarrot = "#d35400";
  fuiDarkAlizarin = "#c0392b";
  fuiDarkClouds = "#bdc3c7";
  fuiDeepClouds = "#dce0e1";
  fuiDarkConcrete = "#7f8c8d";
  bgColorLight = fuiClouds;
  fgColorLight = fuiAsphalt;
  bgColorDark = fuiDarkAsphalt;
  fgColorDark = fuiDarkClouds;
in
{
  xsession.enable = true;

  home.packages = with pkgs; [
    haskellPackages.xmobar
  ];

  xsession.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    config = pkgs.writeText "xmonad.hs" ''
      import Graphics.X11.ExtraTypes.XF86
      import Graphics.X11.Types
      import XMonad
      import XMonad.Hooks.DynamicLog
      import XMonad.Hooks.ManageDocks
      import XMonad.Layout.NoBorders
      import XMonad.Layout.Spacing
      import XMonad.Util.EZConfig
      import XMonad.Util.Scratchpad
      import XMonad.Prompt
      import XMonad.Prompt.Pass
      import System.Environment

      import qualified XMonad.StackSet as W

      myBorderSpacing = spacingRaw False (Border 4 4 4 4) True (Border 4 4 4 4) True

      setPassDir :: IO ()
      setPassDir = setEnv "PASSWORD_STORE_DIR" "${config.home.homeDirectory}/Dropbox/passwords"

      main = do
        setPassDir
        xmonad =<< statusBar "xmobar" myPP toggleStrutsKey myConfig

      -- Command to launch the bar.
      myPP = xmobarPP
          { ppCurrent = xmobarColor "${fgColorLight}" ""
          , ppHidden = xmobarColor "${fuiDarkConcrete}" ""
          , ppLayout = const ""
          , ppTitle = const ""
          , ppSort = fmap (.scratchpadFilterOutWorkspace) $ ppSort defaultPP
          }
      toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

      manageScratchPad :: ManageHook
      manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)
        where
          h = 0.6
          w = 0.8
          t = 0.2
          l = 0.1

      xpconfig :: XPConfig
      xpconfig = def
          { font = "xft:Iosevka:size=12"
          , bgColor = "${bgColorLight}"
          , fgColor = "${fgColorLight}"
          , fgHLight = "${fgColorLight}"
          , bgHLight = "${fuiCarrot}"
          , borderColor = "${fgColorLight}"
          , promptBorderWidth = 4
          , position = CenteredAt 0.5 0.5
          , height = 30
          , maxComplRows = Just 1
          , showCompletionOnTab = False
          }

      myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

      -- Main configuration, override the defaults to your liking.
      myConfig = defaultConfig
          { terminal = "alacritty"
          , borderWidth = 3
          , layoutHook = avoidStruts $ myBorderSpacing $ layoutHook defaultConfig
          , manageHook = manageHook defaultConfig <+> manageDocks <+> manageScratchPad
          , startupHook = startup
          , normalBorderColor = "${fuiDarkClouds}"
          , focusedBorderColor = "${fuiDeepAsphalt}"
          , workspaces = myWorkspaces
          } `additionalKeysP`
          ( [ ("M-p", spawn "rofi -show run")
            , ("<F12>", scratchpadSpawnActionCustom "alacritty --class scratchpad")
            , ("M-;", scratchpadSpawnActionCustom "alacritty --class scratchpad")
            , ("S-M-p p", passPrompt xpconfig)
            , ("S-M-p t", passTypePrompt xpconfig)
            , ("S-M-l", spawn "i3lock")
            ] ++ [
              (mask ++ "M-" ++ [key], screenWorkspace scr >>= flip whenJust (windows . action))
              | (key, scr)  <- zip "ew" [1,0]
            , (action, mask) <- [ (W.view, "") , (W.shift, "S-")]
            ]--  ++ [ (otherModMasks ++ "M-" ++ [key], action tag)
             --  | (tag, key)  <- zip myWorkspaces "123456789"
             --  , (otherModMasks, action) <- [ ("", windows . W.view) -- was W.greedyView
             --                               , ("S-", windows . W.shift)]
             --  ]
          ) `additionalKeys`
          [ ((0, xF86XK_AudioMute), spawn "amixer set Master toggle; amixer set Speaker unmute; amixer set Headphone unmute") -- hack: "toggle" mutes master and individual channels, but only unmutes master
          , ((0, xF86XK_AudioLowerVolume), spawn "amixer sset Master 10%-")
          , ((0, xF86XK_AudioRaiseVolume), spawn "amixer sset Master 10%+")
          , ((0, xF86XK_MonBrightnessUp), spawn "light -A 10")
          , ((0, xF86XK_MonBrightnessDown), spawn "light -U 10")
          , ((0, xF86XK_ScreenSaver), spawn "i3lock")
          ]

      startup :: X ()
      startup = do
        spawn "xsetroot -solid '${fuiClouds}'"
    '';
  };

  home.file.".xmobarrc".text = ''
    Config {
         font =         "xft:Iosevka:size=10,Font Awesome 5 Free Regular:size=9,Font Awesome 5 Free Solid:size=9,Font Awesome 5 Brands:size=9"
       , bgColor =      "${bgColorLight}"
       , fgColor =      "${fgColorLight}"
       , border = BottomB
       , borderColor = "${fuiDarkClouds}"
       , borderWidth = 2
       , position =     Top
       , template = " %StdinReader% }{  %dropbox% |  %KBOS% | %default:Master% |  %wlp3s0wi% |  %battery% |  %date% "
       , allDesktops =      True    -- show on all desktops
       , commands =
            [ Run Weather "KBOS"    [ "-t" , "<fc=${fgColorLight}><tempF></fc>°F"
                                    ] 36000
            , Run Battery           [ "-t" , "<acstatus>"
                                    , "-L" , "20"        -- units: %
                                    , "-H" , "75"        -- units: %
                                    , "-l" , "${fuiAlizarin}"
                                    , "-n" , "${fuiCarrot}"
                                    , "-h" , "${fuiEmerald}"
                                    , "--"
                                    , "-o" , "<left>% (<fc=${fuiCarrot}><timeleft></fc>)"
                                    , "-O" , "<fc=${fuiEmerald}>Charging</fc>"
                                    , "-i" , "<fc=${fuiAlizarin}>Charged</fc>"
                                    ] 50
            , Run Date              "<fc=#6272A4>%F</fc>  %T" "date" 1
            , Run Volume "default" "Master"
                                    [ "-t"      , "<status> <volume>%"
                                    , "--"
                                    , "-O"      , ""
                                    , "-C"      , "${fgColorLight}"
                                    , "-o"      , ""
                                    , "-c"      , "${fgColorLight}"
                                    ] 10
            , Run Wireless "wlp3s0" [ "-t"      , "<essid>"
                                    , "--"
                                    ] 10
            , Run Com "/home/zjn/bin/xmobar-dropbox" [] "dropbox" 10
            , Run StdinReader
            ]
       }
  '';

  home.file."bin/xmobar-dropbox" = {
    text = ''
      #!/bin/sh

      status="$(dropbox status)"

      case "$status" in
              "Up to date")
                      echo "<fc=${fuiEmerald}></fc>" ;;
              Updating*)
                      echo "<fc=${fuiCarrot}>↯</fc>" ;;
              Starting*)
                      echo "<fc=${fuiCarrot}>↯</fc>" ;;
              Checking*)
                      echo "<fc=${fuiCarrot}>↯</fc>" ;;
              Syncing*)
                      echo "<fc=${fuiCarrot}>↯</fc>" ;;
              *)
                      echo "<fc=${fuiAlizarin}></fc> $status" ;;
      esac
    '';
    executable = true;
  };

  # TODO: don't hardcode full path
  home.file.".config/rasi/dracula.rasi".source = ./dracula.rasi;
  home.file.".config/rasi/flucui-light.rasi".source = ./flucui-light.rasi;
  programs.rofi = {
    enable = true;
    theme = "/home/zjn/.config/rasi/flucui-light.rasi";
    font = "Iosevka 9";
  };
}
