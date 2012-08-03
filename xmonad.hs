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
import System.IO
import qualified Data.Map as M

main = do

  xmobar <- spawnPipe "xmobar"

  let myConfig = baseConfig {
    keys      = myKeys,
    logHook   = myDynLog xmobar,

    workspaces = ["home","mail","web","dev0","dev1","music", "var7","var8", "var9"],

    manageHook = manageDocks <+> manageHook baseConfig,
    layoutHook = avoidStruts  $  layoutHook baseConfig,
    handleEventHook = handleEventHook baseConfig<+> docksEventHook,
    modMask = mod4Mask
    }

  xmonad myConfig

  where
    baseConfig = defaultConfig

    myDynLog h = dynamicLogWithPP defaultPP
                    { ppCurrent = xmobarColor "yellow" "" . wrap "[" "]"
                    , ppTitle   = xmobarColor "green"  "" . shorten 150
                    , ppVisible = wrap "(" ")"
                    , ppHiddenNoWindows = xmobarColor "gray" ""
                    , ppHidden  = xmobarColor "lightGreen" ""
                    , ppSep     = "  --  "
                    , ppOutput  = hPutStrLn h
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
          ((modMask x, xK_F7 ), windowPromptBring defaultXPConfig)
        ]
      ]
