import XMonad
import XMonad.Config.Azerty
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Prompt
import XMonad.Prompt.XMonad
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Prompt.Theme
import XMonad.Prompt.Window
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders
import System.IO
import Graphics.X11.ExtraTypes.XF86
import qualified Data.Map as M

main = do
  myConfigWithBar <- statusBar "xmobar" myPP toggleStrutsKey myConfig
  xmonad myConfigWithBar

  where
    baseConfig = defaultConfig

    myConfig = baseConfig {
      keys      = myKeys,

      workspaces = ["1-home","2-mail","3-web","4-dev","5-dev","6-var", "7-var","8-var", "9-music"],

      manageHook =  composeAll [isFullscreen --> doFullFloat] <+> manageDocks <+> manageHook baseConfig,
      layoutHook = smartBorders $ avoidStruts  $ layoutHook baseConfig,
      handleEventHook = handleEventHook baseConfig<+> docksEventHook,
      modMask = mod4Mask
      }
    
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
          ((modMask x, xK_F3 ), shellPrompt  defaultXPConfig),
          ((modMask x, xK_F4 ), sshPrompt    defaultXPConfig),
          ((modMask x, xK_F5 ), themePrompt       defaultXPConfig),
          ((modMask x, xK_F6 ), windowPromptGoto  defaultXPConfig),
          ((modMask x, xK_F7 ), windowPromptBring defaultXPConfig),
          ((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 1%-"),
          ((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 1%+"),
          ((0, xF86XK_AudioMute), spawn "amixer sset Master toggle"),
          ((0, xF86XK_AudioPrev), spawn "mpc prev"),
          ((0, xF86XK_AudioNext), spawn "mpc next"),
          ((0, xF86XK_AudioStop), spawn "mpc stop"),
          ((0, xF86XK_AudioPlay), spawn "mpc toggle")
        ]
      ]
