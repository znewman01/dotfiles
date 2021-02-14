{ config, pkgs, lib, ... }:

let colors = (import ./colors.nix) { lib = lib; };
in {
  xsession.enable = true;

  home.packages = with pkgs; [ haskellPackages.xmobar ];

  xsession.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    config = pkgs.writeText "xmonad.hs" ''
      import Graphics.X11.ExtraTypes.XF86
      import Graphics.X11.Types
      import XMonad
      import qualified XMonad.Hooks.DynamicLog as DLog
      import qualified XMonad.Hooks.DynamicBars as Bars
      import qualified XMonad.Hooks.ManageDocks as Docks
      import XMonad.Layout.NoBorders
      import XMonad.Layout.Spacing
      import XMonad.Util.EZConfig
      import qualified XMonad.Util.Run as Run
      import XMonad.Util.NamedScratchpad
      import XMonad.Util.WorkspaceCompare
      import XMonad.Prompt
      import XMonad.Prompt.Pass
      import System.Environment

      import qualified XMonad.StackSet as W

      myBorderSpacing = spacingRaw False (Border 4 4 4 4) True (Border 4 4 4 4) True

      setPassDir :: IO ()
      setPassDir = setEnv "PASSWORD_STORE_DIR" "${config.home.homeDirectory}/Dropbox/passwords"

      main = do
        setPassDir
        xmonad myConfig

      scratchpads = [NS "terminal" "alacritty --title scratchpad" (title =? "scratchpad") (customFloating $ W.RationalRect 0.1 0.2 0.8 0.6)]

      -- Command to launch the bar.
      myLogPP = DLog.xmobarPP
           { DLog.ppCurrent = DLog.xmobarColor "#${colors.base05}" ""
           , DLog.ppVisible = const ""
           , DLog.ppHidden = const ""
           , DLog.ppHiddenNoWindows = const ""
           , DLog.ppLayout = const ""
           , DLog.ppSep = " | "
           , DLog.ppTitle = id
           , DLog.ppSort = getSortByIndex
           , DLog.ppUrgent = DLog.xmobarColor "#${colors.base08}" ""
           }
      myLogPPActive = myLogPP
          { DLog.ppCurrent = DLog.xmobarColor "#${colors.base09}" ""
          }

      barCreator :: Bars.DynamicStatusBar
      barCreator (S sid) = Run.spawnPipe $ "xmobar --screen " ++ show sid

      barDestroyer :: Bars.DynamicStatusBarCleanup
      barDestroyer = return ()
      -- toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)


      xpconfig :: XPConfig
      xpconfig = def
          { font = "xft:Iosevka:size=12"
          , bgColor =     "#${colors.base01}"
          , fgColor =     "#${colors.base05}"
          , fgHLight =    "#${colors.base05}"
          , bgHLight =    "#${colors.base02}"
          , borderColor = "#${colors.base05}"
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
          , layoutHook = Docks.avoidStruts $ myBorderSpacing $ layoutHook defaultConfig
          , manageHook = manageHook defaultConfig <+> Docks.manageDocks <+> namedScratchpadManageHook scratchpads <+> Docks.manageDocks
          , startupHook = startup
          , logHook = do Bars.multiPP myLogPPActive myLogPP
          , handleEventHook = Bars.dynStatusBarEventHook barCreator barDestroyer <+> Docks.docksEventHook
          , normalBorderColor = "#${colors.base02}"
          , focusedBorderColor = "#${colors.base05}"
          , workspaces = myWorkspaces
          } `additionalKeysP`
          ( [ ("M-p", spawn "rofi -show run")
            , ("<F12>", namedScratchpadAction scratchpads "terminal")
            , ("M-;", namedScratchpadAction scratchpads "terminal")
            , ("S-M-p p", passPrompt xpconfig)
            , ("S-M-p t", passTypePrompt xpconfig)
            , ("M-C-s", sendMessage Docks.ToggleStruts)
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
        spawn "xsetroot -solid '#${colors.base00}'"
        Bars.dynStatusBarStartup barCreator barDestroyer
    '';
  };

  xdg.configFile."xmobar/xmobarrc".text = ''
    Config {
         font =         "xft:Iosevka:size=10,Font Awesome 5 Free Regular:size=9,Font Awesome 5 Free Solid:size=9,Font Awesome 5 Brands:size=9"
       , bgColor =      "#${colors.base01}"
       , fgColor =      "#${colors.base05}"
       , border = BottomB
       , borderColor = "#${colors.base03}"
       , borderWidth = 2
       , position =     Top
       , template = " %StdinReader% }{  %dropbox% |  %KBOS% | %default:Master% |  %wlp3s0wi% |  %battery% |  %date% "
       , allDesktops = True    -- show on all desktops
       , commands =
            [ Run Weather "KBOS"    [ "-t" , "<fc=#${colors.base06}><tempF></fc>°F"
                                    ] 36000
            , Run Battery           [ "-t" , "<acstatus>"
                                    , "-L" , "20"        -- units: %
                                    , "-H" , "75"        -- units: %
                                    , "-l" , "#${colors.base08}"
                                    , "-n" , "#${colors.base05}"
                                    , "-h" , "#${colors.base03}"
                                    , "--"
                                    , "-o" , "<left>% (<fc=${colors.base09}><timeleft></fc>)"
                                    , "-O" , "<fc=#${colors.base05}>Charging</fc>"
                                    , "-i" , "<fc=#${colors.base03}>Charged</fc>"
                                    ] 50
            , Run Date              "<fc=#${colors.base05}>%F</fc>  %T" "date" 1
            , Run Volume "default" "Master"
                                    [ "-t"      , "<status> <volume>%"
                                    , "--"
                                    , "-O"      , ""
                                    , "-C"      , "#${colors.base05}"
                                    , "-o"      , ""
                                    , "-c"      , "#${colors.base05}"
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
                      echo "<fc=#${colors.base03}></fc>" ;;
              Updating*)
                      echo "<fc=#${colors.base0A}>↯</fc>" ;;
              Starting*)
                      echo "<fc=#${colors.base0A}>↯</fc>" ;;
              Checking*)
                      echo "<fc=#${colors.base0A}>↯</fc>" ;;
              Syncing*)
                      echo "<fc=#${colors.base0A}>↯</fc>" ;;
              *)
                      echo "<fc=#${colors.base08}></fc> $status" ;;
      esac
    '';
    executable = true;
  };

  xdg.configFile."rasi/base16.rasi".text = ''
    * {
        red:                         rgba ( ${colors.r colors.base08},
                                            ${colors.g colors.base08},
                                            ${colors.b colors.base08},
                                            100 % );
        blue:                        rgba ( ${colors.r colors.base0D},
                                            ${colors.g colors.base0D},
                                            ${colors.b colors.base0D},
                                            100 % );
        lightfg:                     rgba ( ${colors.r colors.base06},
                                            ${colors.g colors.base06},
                                            ${colors.b colors.base06},
                                            100 % );
        lightbg:                     rgba ( ${colors.r colors.base01},
                                            ${colors.g colors.base01},
                                            ${colors.b colors.base01},
                                            100 % );
        foreground:                  rgba ( ${colors.r colors.base05},
                                            ${colors.g colors.base05},
                                            ${colors.b colors.base05},
                                            100 % );
        background:                  rgba ( ${colors.r colors.base00},
                                            ${colors.g colors.base00},
                                            ${colors.b colors.base00},
                                            100 % );
        background-color:            rgba ( ${colors.r colors.base00},
                                            ${colors.g colors.base00},
                                            ${colors.b colors.base00},
                                            0 % );
        separatorcolor:              @foreground;
        border-color:                @foreground;
        selected-normal-foreground:  @lightbg;
        selected-normal-background:  @lightfg;
        selected-active-foreground:  @background;
        selected-active-background:  @blue;
        selected-urgent-foreground:  @background;
        selected-urgent-background:  @red;
        normal-foreground:           @foreground;
        normal-background:           @background;
        active-foreground:           @blue;
        active-background:           @background;
        urgent-foreground:           @red;
        urgent-background:           @background;
        alternate-normal-foreground: @foreground;
        alternate-normal-background: @lightbg;
        alternate-active-foreground: @blue;
        alternate-active-background: @lightbg;
        alternate-urgent-foreground: @red;
        alternate-urgent-background: @lightbg;
        spacing:                     2;
    }
    window {
        background-color: @background;
        border:           1;
        padding:          5;
    }
    mainbox {
        border:           0;
        padding:          0;
    }
    message {
        border:           1px solid 0px 0px ;
        border-color:     @separatorcolor;
        padding:          1px ;
    }
    textbox {
        text-color:       @foreground;
    }
    listview {
        fixed-height:     0;
        border:           2px solid 0px 0px ;
        border-color:     @separatorcolor;
        spacing:          2px ;
        scrollbar:        true;
        padding:          2px 0px 0px ;
    }
    element {
        border:           0;
        padding:          1px ;
    }
    element normal.normal {
        background-color: @normal-background;
        text-color:       @normal-foreground;
    }
    element normal.urgent {
        background-color: @urgent-background;
        text-color:       @urgent-foreground;
    }
    element normal.active {
        background-color: @active-background;
        text-color:       @active-foreground;
    }
    element selected.normal {
        background-color: @selected-normal-background;
        text-color:       @selected-normal-foreground;
    }
    element selected.urgent {
        background-color: @selected-urgent-background;
        text-color:       @selected-urgent-foreground;
    }
    element selected.active {
        background-color: @selected-active-background;
        text-color:       @selected-active-foreground;
    }
    element alternate.normal {
        background-color: @alternate-normal-background;
        text-color:       @alternate-normal-foreground;
    }
    element alternate.urgent {
        background-color: @alternate-urgent-background;
        text-color:       @alternate-urgent-foreground;
    }
    element alternate.active {
        background-color: @alternate-active-background;
        text-color:       @alternate-active-foreground;
    }
    scrollbar {
        width:            4px ;
        border:           0;
        handle-color:     @normal-foreground;
        handle-width:     8px ;
        padding:          0;
    }
    sidebar {
        border:           2px solid 0px 0px ;
        border-color:     @separatorcolor;
    }
    button {
        spacing:          0;
        text-color:       @normal-foreground;
    }
    button selected {
        background-color: @selected-normal-background;
        text-color:       @selected-normal-foreground;
    }
    inputbar {
        spacing:          0px;
        text-color:       @normal-foreground;
        padding:          1px ;
        children:         [ prompt,textbox-prompt-colon,entry,case-indicator ];
    }
    case-indicator {
        spacing:          0;
        text-color:       @normal-foreground;
    }
    entry {
        spacing:          0;
        text-color:       @normal-foreground;
    }
    prompt {
        spacing:          0;
        text-color:       @normal-foreground;
    }
    textbox-prompt-colon {
        expand:           false;
        str:              ":";
        margin:           0px 0.3000em 0.0000em 0.0000em ;
        text-color:       inherit;
    }
  '';

  programs.rofi = {
    enable = true;
    theme = "/home/zjn/.config/rasi/base16.rasi";
    font = "Iosevka 9";
  };
}
