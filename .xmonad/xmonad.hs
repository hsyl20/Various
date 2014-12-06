import XMonad
import XMonad.Config.Azerty
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.XMonad
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Prompt.Theme
import XMonad.Prompt.Window
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders
import XMonad.Util.Run
import qualified XMonad.StackSet as W

import Data.Char (isSpace,isControl)
import Data.Maybe (fromMaybe, listToMaybe)
import System.IO 
import System.Process
import Graphics.X11.ExtraTypes.XF86
import System.Posix.IO
import System.Posix.Process (executeFile)
import Codec.Binary.UTF8.String
import Control.Monad (unless)
import qualified Data.Map as Map
import Data.Char (toUpper)

main = do
  myConfigWithBar <- statusBar "xmobar" myPP toggleStrutsKey myConfig
  xmonad myConfigWithBar

-- Configuration
myConfig = defaultConfig {
   keys          = keys',
   mouseBindings = mouseBindings',
   workspaces = workspaces',
   manageHook = manageHook',
   layoutHook = layoutHook',
   handleEventHook = handleEventHook',
   modMask = mod4Mask
}

-- Names of the workspaces
workspaces' = ["uno","dos","tres","cuatro","cinco"
              ,"seis", "siete","ocho", "nueve"]

-- Action when a new window is opened
manageHook'= manageDocks <+> composeAll [
     isFullscreen --> doFullFloat,
     className =? "Firefox" --> doShift "tres",
     className =? "Thunderbird" --> doShift "dos",
     className =? "Empathy" --> doShift "dos"
  ] 

-- Available layouts
layoutHook' = avoidStruts $ smartBorders $ layoutHook defaultConfig

-- Handle X events
handleEventHook' = docksEventHook <+> handleEventHook defaultConfig <+> docksEventHook

-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

myPP = defaultPP
              { ppCurrent = wrap "[" "]" -- xmobarColor "white" "" -- . wrap "[" "]"
              , ppTitle   = xmobarColor "#5252DD"  "" . shorten 200
              , ppVisible = wrap "(" ")"
              , ppHiddenNoWindows = xmobarColor "#333333" ""
              , ppHidden  = xmobarColor "#888888" ""
              , ppUrgent  = map toUpper
              , ppSep     = " | "
              , ppOrder   = \ [workspaces,layout,title] -> [workspaces,title]
              }

-- Aditional key bindings
keys' x = Map.unions [
      Map.fromList $ [
             ((modMask x .|. shiftMask, xK_Return),   spawn "/home/hsyl20/.usr/bin/xterm_pwd"),
             ((modMask x .|. shiftMask, xK_z),        spawn "systemctl hibernate"),
             ((modMask x .|. shiftMask, xK_F12),      spawn "systemctl poweroff"),
             --((modMask x .|. shiftMask, xK_F12),      xmonadPrompt defaultXPConfig),
             ((modMask x, xK_x ),                     shellPrompt defaultXPConfig),
             ((modMask x .|. shiftMask, xK_F11 ),     spawn "slock"),
             ((modMask x .|. shiftMask, xK_F4 ),      sshPrompt defaultXPConfig),
             ((modMask x .|. shiftMask, xK_F5 ),      themePrompt defaultXPConfig),
             ((modMask x .|. shiftMask, xK_F6 ),      windowPromptGoto defaultXPConfig),
             ((modMask x .|. shiftMask, xK_F7 ),      windowPromptBring defaultXPConfig),
             ((0, xF86XK_AudioLowerVolume),           spawn "amixer set Master 1%-"),
             ((0, xF86XK_AudioRaiseVolume),           spawn "amixer set Master 1%+"),
             ((0, xF86XK_AudioMute),                  spawn "amixer sset Master toggle"),
             ((0, xF86XK_AudioPrev),                  spawn "mpc prev"),
             ((0, xF86XK_AudioNext),                  spawn "mpc next"),
             ((0, xF86XK_AudioStop),                  spawn "mpc stop"),
             ((0, xF86XK_AudioPlay),                  spawn "mpc toggle"),
             ((0, xK_Print),                          spawn "/home/hsyl20/.usr/bin/screenshot scr"),
             ((shiftMask, xK_Print),                  spawn "/home/hsyl20/.usr/bin/screenshot win")],

       
      -- mod-[F1..F9] %! Switch to workspace N
      -- mod-shift-[F1..F9] %! Move client to workspace N
      Map.fromList [((m .|. modMask x, k), windows $ f i)
        | (i, k) <- zip workspaces' [xK_F1 .. xK_F9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]],

      keys defaultConfig x,
      azertyKeys x
   ]

-- Additional mouse bindings
mouseBindings' = mouseBindings defaultConfig
