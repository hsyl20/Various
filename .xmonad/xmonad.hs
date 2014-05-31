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
workspaces' = ["1-home","2-mail","3-web","4-dev","5-dev","6-misc", "7-misc","8-misc", "9-music"]

-- Action when a new window is opened
manageHook'= manageDocks <+> composeAll [
     isFullscreen --> doFullFloat,
     className =? "Firefox" --> doShift "3-web",
     className =? "Thunderbird" --> doShift "2-mail",
     className =? "Empathy" --> doShift "2-mail"
  ] 

-- Available layouts
layoutHook' = avoidStruts $ smartBorders $ layoutHook defaultConfig

-- Handle X events
handleEventHook' = docksEventHook <+> handleEventHook defaultConfig <+> docksEventHook

-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

myPP = defaultPP
              { ppCurrent = xmobarColor "yellow" "" . wrap "[" "]"
              , ppTitle   = xmobarColor "green"  "" . shorten 150
              , ppVisible = wrap "(" ")"
              , ppHiddenNoWindows = xmobarColor "gray" ""
              , ppHidden  = xmobarColor "lightGreen" ""
              , ppSep     = "  --  "
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
