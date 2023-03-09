{-# LANGUAGE PatternGuards #-}

import XMonad
import XMonad.Config.Azerty
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.NoBorders
import XMonad.Util.Run
import qualified XMonad.StackSet as W

import Graphics.X11.ExtraTypes.XF86
import qualified Data.Map as Map
import Text.Read (readMaybe)
import Data.Char

main = do
  -- "-x 1" = start on screen 1
  myConfigWithBar <- statusBar "xmobar -x 1" myPP toggleStrutsKey myConfig
  xmonad myConfigWithBar

-- Configuration
myConfig = def
   { keys              = keys'
   , mouseBindings     = mouseBindings'
   , workspaces        = workspaces'
   , manageHook        = manageHook'
   , layoutHook        = layoutHook'
   , handleEventHook   = handleEventHook'
   , modMask           = mod4Mask
   , focusFollowsMouse = True
   }

-- Names of the workspaces
workspaces' =
   [ " 1 "
   , " 2 "
   , " 3 "
   , " 4 "
   , " 5 "
   , " 6 "
   , " 7 "
   , " 8 "
   , " 9 "
   ]

-- Action when a new window is opened
manageHook'= manageDocks <+> composeAll
   [ isFullscreen                      --> doFullFloat
   , className =? "qemu-system-x86_64" --> doCenterFloat
   , className =? "Qemu-system-x86_64" --> doCenterFloat
   ] 

-- Available layouts
layoutHook' = avoidStruts $ smartBorders $ layoutHook def

-- Handle X events
handleEventHook'
   =   docksEventHook
   <+> handleEventHook def
   <+> fullscreenEventHook

-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = mMask} = (mMask, xK_b)

myPP = def
   { ppCurrent = xmobarColor "black" "#00E000" -- . wrap "[" "]"
   , ppTitle   = xmobarColor "lightBlue"  "" . shorten 120 -- max title length (otherwise other fields on the right are truncated)
   , ppVisible = xmobarColor "black" "#009900" -- . wrap "(" ")"
   , ppHiddenNoWindows = xmobarColor "gray" "#220000"
   , ppHidden  = xmobarColor "gray" "#444400"
   , ppSep     = "  |  "
   }

promptConfig = def
   { font              = "xft:Terminus:pixelsize=14"
   , height            = 24
   , promptBorderWidth = 1
   , historyFilter     = deleteConsecutive
   }

-- Aditional key bindings
keys' x = Map.unions
   [ Map.fromList 
      [ ((modMask x .|. shiftMask, xK_Return),   spawn "alacritty")
      , ((modMask x .|. shiftMask, xK_z),        spawn "systemctl hibernate")
      , ((modMask x .|. shiftMask, xK_F12),      spawn "systemctl poweroff")
      --, ((modMask x .|. shiftMask, xK_F12),      xmonadPrompt defaultXPConfig)
      , ((modMask x, xK_x ),                     shellPrompt promptConfig)
--      , ((modMask x, xK_u ),                     unicodePrompt def)
      , ((modMask x .|. shiftMask , xK_u ),      unicodePromptChar def)
      , ((modMask x .|. shiftMask, xK_F11 ),     spawn "slock")
      --, ((modMask x .|. shiftMask, xK_F4 ),      sshPrompt defaultXPConfig)
      --, ((modMask x .|. shiftMask, xK_F5 ),      themePrompt defaultXPConfig)
      --, ((modMask x .|. shiftMask, xK_F6 ),      windowPromptGoto defaultXPConfig)
      --, ((modMask x .|. shiftMask, xK_F7 ),      windowPromptBring defaultXPConfig)
      , ((0, xF86XK_AudioLowerVolume),           spawn "amixer set Master 1%-")
      , ((0, xF86XK_AudioRaiseVolume),           spawn "amixer set Master 1%+")
      , ((modMask x .|. shiftMask, xK_KP_Subtract), spawn "amixer set Master 1%-")
      , ((modMask x .|. shiftMask, xK_KP_Add),      spawn "amixer set Master 1%+")
      , ((0, xF86XK_AudioMute),                  spawn "amixer sset Master toggle")
      , ((0, xF86XK_AudioPrev),                  spawn "mpc prev")
      , ((0, xF86XK_AudioNext),                  spawn "mpc next")
      , ((0, xF86XK_AudioStop),                  spawn "mpc stop")
      , ((0, xF86XK_AudioPlay),                  spawn "mpc toggle")
      , ((0, xK_Print),                          spawn "/home/hsyl20/.usr/bin/screenshot scr")
      , ((shiftMask, xK_Print),                  spawn "/home/hsyl20/.usr/bin/screenshot win")
      , ((shiftMask .|. modMask x, xK_Print),    spawn "/home/hsyl20/.usr/bin/screenshot area")
      ]
   
     -- mod-[F1..F9] %! Switch to workspace N
     -- mod-shift-[F1..F9] %! Move client to workspace N
   , Map.fromList
      [((m .|. modMask x, k), windows $ f i)
        | (i, k) <- zip workspaces' [xK_F1 .. xK_F9]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
      ]

   , keys def x

   , azertyKeys x
       
   ]

-- Additional mouse bindings
mouseBindings' = mouseBindings def

----------------------
-- Unicode char prompt

-- | Prompt the user for a unicode character to be inserted into the paste buffer of the X server.
unicodePromptChar :: XPConfig -> X ()
unicodePromptChar cfg = mkXPrompt UnicodeChar cfg unicodeCompl paste
  where
    unicodeCompl s
      | Just n <- readMaybe s
      , not (isControl (chr n))
      = return [s ++ " - " ++ [chr n]]
    unicodeCompl _ = return []

    paste [] = return ()
    paste s
      | Just n <- readMaybe s = do
        _ <- runProcessWithInput "xsel" ["-i"] [chr n]
        return ()
      | otherwise = return ()

data UnicodeChar = UnicodeChar
instance XPrompt UnicodeChar where
    showXPrompt UnicodeChar = "Unicode code: "
    commandToComplete UnicodeChar s = s
    nextCompletion UnicodeChar = getNextCompletion

