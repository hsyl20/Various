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

import Data.Char (isSpace,isControl)
import Data.Maybe (fromMaybe, listToMaybe)
import System.IO 
import System.Process
import Graphics.X11.ExtraTypes.XF86
import System.Posix.IO
import System.Posix.Process (executeFile)
import Codec.Binary.UTF8.String
import Control.Monad (unless)
import qualified Data.Map as M

main = do
  myConfigWithBar <- statusBar "xmobar" myPP toggleStrutsKey myConfig
  xmonad myConfigWithBar

  where
    baseConfig = defaultConfig

    myConfig = baseConfig {
      keys      = myKeys,

      workspaces = ["1-home","2-mail","3-web","4-dev","5-dev","6-misc", "7-misc","8-misc", "9-music"],

      manageHook = myManageHook,
      layoutHook = smartBorders $ avoidStruts $ layoutHook baseConfig,
      handleEventHook = handleEventHook baseConfig<+> docksEventHook,
      modMask = mod4Mask
      }

    myManageHook = composeAll [
        isFullscreen --> doFullFloat,
        className =? "Firefox" --> doShift "3-web",
        className =? "Thunderbird" --> doShift "2-mail",
        className =? "Konversation" --> doShift "1-home",
        className =? "Kcalc" --> doFloat
      ] <+> manageDocks <+> manageHook baseConfig
    
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

    myKeys x = M.unions [
      keys baseConfig x,
      azertyKeys x,
      M.fromList $ [
          ((modMask x, xK_F12), xmonadPrompt defaultXPConfig),
          ((modMask x, xK_x ), shellPrompt defaultXPConfig),
          ((modMask x, xK_F11 ), spawn "xlock -mode space"),
          ((modMask x, xK_F4 ), sshPrompt defaultXPConfig),
          ((modMask x, xK_F5 ), themePrompt defaultXPConfig),
          ((modMask x, xK_F6 ), windowPromptGoto defaultXPConfig),
          ((modMask x, xK_F7 ), windowPromptBring defaultXPConfig),
          ((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 1%-"),
          ((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 1%+"),
          ((0, xF86XK_AudioMute), spawn "amixer sset Master toggle"),
          ((0, xF86XK_AudioPrev), spawn "mpc prev"),
          ((0, xF86XK_AudioNext), spawn "mpc next"),
          ((0, xF86XK_AudioStop), spawn "mpc stop"),
          ((0, xF86XK_AudioPlay), spawn "mpc toggle"),
          ((modMask x, xK_F3), calcPrompt defaultXPConfig "calc")
        ]
      ]

calcPrompt :: XPConfig -> String -> X ()
calcPrompt c ans = inputPrompt c ans ?+ \input -> do
  output <- liftIO $ runProcessWithInput "ghc" [" -e \"", input, "\""] []
  calcPrompt c $ filter (not . isControl) $ output
  where
    trim  = f . f
    f = reverse . dropWhile isSpace . dropWhile isControl
