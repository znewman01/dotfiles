{ config, pkgs, lib, ... }:

let colors = (import ./colors) { lib = lib; };
in {
  xsession.enable = true;

  home.packages = with pkgs; [ haskellPackages.xmobar hsetroot rofi-pass ];

  xsession.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    config = pkgs.writeText "xmonad.hs" ''
      import Data.List
      import Data.Char
      import Data.Default
      import Data.Function (on)
      import Graphics.X11.ExtraTypes.XF86
      import Graphics.X11.Types
      import XMonad
      import qualified XMonad.Hooks.DynamicLog as DLog
      import qualified XMonad.Hooks.DynamicBars as Bars
      import qualified XMonad.Hooks.DynamicProperty as DProp
      import qualified XMonad.Hooks.ManageDocks as Docks
      import qualified XMonad.Hooks.FadeInactive as Fade
      import XMonad.Layout.NoBorders
      import XMonad.Layout.Spacing
      import XMonad.Util.EZConfig
      import qualified XMonad.Util.Run as Run
      import XMonad.Util.NamedScratchpad
      import XMonad.Util.SpawnOnce
      import XMonad.Util.WorkspaceCompare
      import qualified XMonad.Actions.PhysicalScreens as PhysicalScreens
      import qualified XMonad.Hooks.EwmhDesktops as Ewmh
      import XMonad.Prompt
      import XMonad.Prompt.Pass
      import System.Environment

      import qualified XMonad.StackSet as W

      myBorderSpacing = spacingRaw False (Border 4 4 4 4) True (Border 4 4 4 4) True

      setPassDir :: IO ()
      setPassDir = setEnv "PASSWORD_STORE_DIR" "${config.home.homeDirectory}/Dropbox/passwords"

      main = do
        setPassDir
        xmonad $ Ewmh.ewmh myConfig

      scratchpads = [NS "terminal" "alacritty --title scratchpad" (title =? "scratchpad") (customFloating $ W.RationalRect 0.1 0.2 0.8 0.6)]

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


      xpconfig :: XPConfig
      xpconfig = def
          { font = "xft:Roboto Mono:size=12"
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

      myManageHook = composeAll . concat $
              [ [ manageHook defaultConfig ]
              , [ Docks.manageDocks ]
              , [ title =? "emacsfloat"  --> (customFloating $ W.RationalRect (1/4) (1/8) (1/2) (3/4)) ]
              , [ title =? "emacslast"  --> doShift "9" ]
              , [ title =? "htop"  --> doShift "1" ]
              , [ namedScratchpadManageHook scratchpads ]
              , [ classMatch app --> doShift "8" | app <- messageApps ]
              , [ title =? "xmessage"  --> (customFloating $ W.RationalRect 0.7 0.1 0.2 0.2) ]
              ]
         where messageApps = ["slack", "signal", "keybase", "skype"]
               classMatch s = fmap (s `isInfixOf`) (fmap (map toLower) className)
      myDynPropHook = composeAll . concat $
          [ [ zoomLike <&&> titleMatch "licensed" --> doShift "8" ]
          , [ zoomLike <&&> titleMatch "meeting" --> doShift "6" ]
          , [ zoomLike <&&> titleMatch "choose one" --> doShift "6" ]
          ]
        where zoomLike = fmap ("zoom" `isInfixOf`) (fmap (map toLower) className)
              titleMatch s = fmap (s `isInfixOf`) (fmap (map toLower) title)



      -- like PhysicalScreens.getNeighbour and friends, but without wrapping
      getNeighborNoWrap :: PhysicalScreens.ScreenComparator -> Int -> X ScreenId
      getNeighborNoWrap (PhysicalScreens.ScreenComparator cmpScreen) d =
        do w <- gets windowset
           let ss = map W.screen $ sortBy (cmpScreen `on` PhysicalScreens.getScreenIdAndRectangle) $ W.current w : W.visible w
               curPos = maybe 0 id $ findIndex (== W.screen (W.current w)) ss
               pos = max 0 . min ((length ss) - 1) $ curPos + d
           return $ ss !! pos

      neighborWindowsNoWrap :: PhysicalScreens.ScreenComparator -> Int -> (WorkspaceId -> WindowSet -> WindowSet) -> X ()
      neighborWindowsNoWrap sc d f = do s <- getNeighborNoWrap sc d
                                        w <- screenWorkspace s
                                        whenJust w $ windows . f

      onPrevNeighborNoWrap :: PhysicalScreens.ScreenComparator -> (WorkspaceId -> WindowSet -> WindowSet) -> X ()
      onPrevNeighborNoWrap sc = neighborWindowsNoWrap sc (-1)

      onNextNeighborNoWrap :: PhysicalScreens.ScreenComparator -> (WorkspaceId -> WindowSet -> WindowSet) -> X ()
      onNextNeighborNoWrap sc = neighborWindowsNoWrap sc 1

      myConfig = defaultConfig
          { terminal = "alacritty"
          , borderWidth = 3
          , layoutHook = Docks.avoidStruts $ myBorderSpacing $ layoutHook defaultConfig
          , manageHook =  myManageHook
          , startupHook = composeAll
              [ Bars.dynStatusBarStartup barCreator barDestroyer
              , spawn "hsetroot -solid '#${colors.base00}'"
              , spawnOnce "slack"
              , spawnOnce "signal-desktop"
              , spawnOnce "skypeforlinux"
              , spawnOnce "zoom-us"
              , spawnOnce "alacritty --title htop --command nix-shell -p htop --command htop"
              , spawnOnce "sleep 1; keybase-gui"
              , spawnOnce "sleep 4; emacsclient --frame-parameters='(quote (name . \"emacslast\"))' --eval '(org-agenda nil \"n\")' -c"
              ]
          , logHook = composeAll
              [ Bars.multiPP myLogPPActive myLogPP
              , Fade.fadeInactiveLogHook 0xbbbbbbbb
              ]
          , handleEventHook = composeAll
              [ Bars.dynStatusBarEventHook barCreator barDestroyer
              , Docks.docksEventHook
              , DProp.dynamicPropertyChange "WM_NAME" myDynPropHook
              ]
          , normalBorderColor = "#${colors.base02}"
          , focusedBorderColor = "#${colors.base02}"  -- can be the same with window fade
          , workspaces = myWorkspaces
          } `additionalKeysP`
            [ ("M-p", spawn "rofi -show run")
            , ("M-;", namedScratchpadAction scratchpads "terminal")
            , ("S-M-p", spawn "rofi-pass")
            , ("S-M-r", spawn "record_screen.sh")  -- shadows 3-monitor bindings but I just make w/e left/right
            , ("C-S-M-r", spawn "ARG=$(echo -e 'selection\nwindow\nall\nfullscreen' | rofi -dmenu -no-custom -p 'Record') && record_screen.sh $ARG")
            , ("<Print>", spawn "screenshot.sh -s")
            , ("M-<Print>", spawn "screenshot_rofi.sh -s")
            , ("S-M-s", spawn "share.sh")
            , ("C-S-M-s", spawn "mimeify_clipboard.sh")
            , ("S-M-l", spawn "i3lock -c ${colors.base00}")
            , ("S-M-c",
                 spawn "rofi -show calc -modi calc -no-show-match -no-sort -lines 0 -calc-command \"xdotool type '{result}'\" -kb-accept-custom 'Return' -kb-accept-entry \'\'")
            , ("S-M-C-c",
                 spawn "rofi -show calc -modi calc -no-show-match -no-sort -lines 0 -calc-command \"xdotool type '{result}'\" -kb-accept-custom 'Return' -kb-accept-entry \'\' -filter \"$(xclip -o -sel primary)\"")
            , ("S-M-d", kill)
            , ("M-w", onPrevNeighborNoWrap PhysicalScreens.horizontalScreenOrderer W.view)
            , ("S-M-w", onPrevNeighborNoWrap PhysicalScreens.horizontalScreenOrderer $ W.shift)
            , ("M-e", onNextNeighborNoWrap PhysicalScreens.horizontalScreenOrderer $ W.view)
            , ("S-M-e", onNextNeighborNoWrap PhysicalScreens.horizontalScreenOrderer $ W.shift)
            , ("<XF86AudioMute>", spawn "amixer set Master toggle; amixer set Speaker unmute; amixer set Headphone unmute") -- hack: "toggle" mutes master and individual channels, but only unmutes master
            , ("<XF86AudioLowerVolume>", spawn "amixer sset Master 10%-")
            , ("<XF86AudioRaiseVolume>", spawn "amixer sset Master 10%+")
            , ("<XF86MonBrightnessUp>", spawn "light -A 10")
            , ("<XF86MonBrightnessDown>", spawn "light -U 10")
            , ("<XF86ScreenSaver>", spawn "i3lock -c ${colors.base00}")
            ]

    '';
  };

  home.file."bin/em-float" = {
    text = ''
      #!/bin/sh
      emacsclient --frame-parameters='(quote (name . "emacsfloat"))' -c $@
    '';
    executable = true;
  };

  xdg.configFile."xmobar/xmobarrc".text = ''
    Config {
         font =         "xft:Roboto Mono:size=12,Font Awesome 5 Free Regular:size=9,Font Awesome 5 Free Solid:size=9,Font Awesome 5 Brands:size=9"
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
                                    , "-o" , "<left>% (<fc=#${colors.base05}><timeleft></fc>)"
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

  xdg.configFile."rofi-pass/config".text = ''
    BROWSER='xdg-open'
    EDITOR='em-float'
    default_do='autotype'
    copy_pass='Ctrl+c'
    copy_menu="Alt+C"
    type_pass='Ctrl+t'
    type_menu="Ctrl+T"
    default_user='znewman01'
    open_url='Ctrl+o'
    action_menu='Ctrl+A'
    insert_pass='Ctrl+i'
    show='Ctrl+s'
    clip=clipboard
    clip_clear=45
  '';

  programs.rofi = {
    enable = true;
    theme = "/home/zjn/.config/rasi/base16.rasi";
    package = with pkgs; rofi.override { plugins = [ rofi-calc ]; };
    font = "Roboto Mono 12";
    extraConfig = "rofi.m: -4";
  };

  home.file."bin/record_screen.sh".source = ./record_screen.sh;
  home.file."bin/screenshot.sh".source = ./screenshot.sh;
  home.file."bin/screenshot_rofi.sh" = {
    executable = true;
    text = ''
      #!/usr/bin/env bash
      set -eo pipefail
      ARG=$(echo -e 'selection\nwindow\nfullscreen\nall' | rofi -dmenu -no-custom -p 'Screenshot')
      if [ $ARG == "selection" ]; then
        screenshot.sh -s
      elif [ $ARG == "window" ]; then
        screenshot.sh -i $(xdotool getactivewindow)
      elif [ $ARG == "fullscreen" ]; then
        screenshot.sh fullscreen
      elif [ $ARG == "all" ]; then
        screenshot.sh '#${colors.base00}'
      fi
    '';
  };
  home.file."bin/mimeify_clipboard.sh" = {
    executable = true;
    text = ''
      #!/usr/bin/env bash
      set -e
      FILE=$(xclip -selection clipboard -o)
      if [ -f "$FILE" ]; then
          MIME=$(xdg-mime query filetype "$FILE")
          xclip -t $MIME "$FILE" -selection clipboard
          notify-send "Converted $FILE to $MIME in clipboard"
      else
          notify-send "Clipboard contents not a file that exists: $(echo $FILE | head -c 100)"
      fi
    '';
  };
  home.file."bin/share.sh".source = ./share.sh;
}
