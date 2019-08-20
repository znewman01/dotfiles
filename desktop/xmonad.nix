{ config, pkgs, ... }:

let
  # TODO: make module
  bgColor = "#282A36";
  fgColor = "#F8F8F2";
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
          { ppCurrent = xmobarColor "${fgColor}" ""
          , ppHidden = xmobarColor "#6272A4" ""
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
          { font = "xft:Hack:size=9"
          , bgColor = "${bgColor}"
          , fgColor = "${fgColor}"
          , fgHLight = "${fgColor}"
          , bgHLight = "#44475a"
          , borderColor = "${fgColor}"
          , promptBorderWidth = 4
          , position = CenteredAt 0.5 0.5
          , height = 30
          , maxComplRows = Just 1
          , showCompletionOnTab = False
          }

      -- Main configuration, override the defaults to your liking.
      myConfig = defaultConfig
          { terminal = "alacritty"
          , borderWidth = 3
          , layoutHook = avoidStruts $ myBorderSpacing $ layoutHook defaultConfig
          , manageHook = manageHook defaultConfig <+> manageDocks <+> manageScratchPad
          , startupHook = startup
          , normalBorderColor = "${bgColor}"
          , focusedBorderColor = "${fgColor}"
          } `additionalKeysP`
          ( [ ("M-p", spawn "rofi -show run")
            , ("<F12>", scratchpadSpawnActionCustom "alacritty --class scratchpad")
            , ("S-M-p p", passPrompt xpconfig)
            , ("S-M-p t", passTypePrompt xpconfig)
            , ("S-M-l", spawn "i3lock")
            ] ++ [
              (mask ++ "M-" ++ [key], screenWorkspace scr >>= flip whenJust (windows . action))
              | (key, scr)  <- zip "we" [1,0]
            , (action, mask) <- [ (W.view, "") , (W.shift, "S-")]
            ]
          ) `additionalKeys`
          [ ((0, xF86XK_AudioMute), spawn "amixer set Master toggle; amixer set Speaker unmute; amixer set Headphone unmute") -- hack: "toggle" mutes master and individual channels, but only unmutes master
          , ((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 2-")
          , ((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 2+")
          , ((0, xF86XK_MonBrightnessUp), spawn "light -A 10")
          , ((0, xF86XK_MonBrightnessDown), spawn "light -U 10")
          , ((0, xF86XK_ScreenSaver), spawn "i3lock")
          ]

      startup :: X ()
      startup = do
        spawn "xsetroot -solid '#44475A'"
    '';
  };

  home.file.".xmobarrc".text = ''
    Config {
         font =         "xft:Hack:size=9,Font Awesome 5 Free Regular:size=9,Font Awesome 5 Free Solid:size=9,Font Awesome 5 Brands:size=9"
       , bgColor =      "${bgColor}"
       , fgColor =      "${fgColor}"
       , position =     Top
       , template = " %StdinReader% }{ %multicpu% |  %dropbox% |  %KBOS% | %default:Master% |  %wlp3s0wi% |  %battery% |  %date% "
       , allDesktops =      True    -- show on all desktops
       , commands =
            [ Run Weather "KBOS"    [ "-t" , "<fc=#6272A4><tempF></fc>°F"
                                    ] 36000
            , Run MultiCpu          [ "-t" , " <total0>%  <total1>%"
                                    , "-L" , "50"         -- units: %
                                    , "-H" , "85"         -- units: %
                                    , "-l" , "#50FA7B"
                                    , "-n" , "#FFB86C"
                                    , "-h" , "#FF5555"
                                    , "-p" , "3"
                                    ] 10
            , Run Battery           [ "-t" , "<acstatus>"
                                    , "-L" , "20"        -- units: %
                                    , "-H" , "75"        -- units: %
                                    , "-l" , "#FF5555"
                                    , "-n" , "#FFB86C"
                                    , "-h" , "#50FA7B"
                                    , "--"
                                    , "-o" , "<left>% (<fc=#6272A4><timeleft></fc>)"
                                    , "-O" , "<fc=#FFB86C>Charging</fc>"
                                    , "-i" , "<fc=#FF5555>Charged</fc>"
                                    ] 50
            , Run Date              "<fc=#6272A4>%F</fc>  %T" "date" 1
            , Run Volume "default" "Master"
                                    [ "-t"      , "<status> <volume>%"
                                    , "--"
                                    , "-O"      , ""
                                    , "-C"      , "${fgColor}"
                                    , "-o"      , ""
                                    , "-c"      , "${fgColor}"
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
                      echo "<fc=#50FA7B></fc>" ;;
              Updating*)
                      echo "<fc=#FFB86C>↯</fc>" ;;
              Starting*)
                      echo "<fc=#FFB86C>↯</fc>" ;;
              Checking*)
                      echo "<fc=#FFB86C>↯</fc>" ;;
              Syncing*)
                      echo "<fc=#FFB86C>↯</fc>" ;;
              *)
                      echo "<fc=#FF5555></fc> $status" ;;
      esac
    '';
    executable = true;
  };

  # TODO: don't hardcode full path
  home.file.".config/rasi/dracula.rasi".source = ./dracula.rasi;
  programs.rofi = {
    enable = true;
    theme = "/home/zjn/.config/rasi/dracula.rasi";
    font = "Hack 9";
  };
}
