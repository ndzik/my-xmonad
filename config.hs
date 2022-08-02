module Main where

import Control.Monad (filterM)
import qualified Data.Map as M
import XMonad
  ( ButtonMask,
    Default (def),
    Full (Full),
    KeySym,
    Layout,
    MonadState (get),
    Query,
    WorkspaceId,
    X,
    XConfig
      ( borderWidth,
        keys,
        layoutHook,
        manageHook,
        modMask,
        terminal,
        workspaces
      ),
    XState (windowset),
    className,
    composeAll,
    doFloat,
    gets,
    handleEventHook,
    mod4Mask,
    runQuery,
    spawn,
    title,
    xmonad,
    (-->),
    (<+>),
    (=?),
    (|||),
  )
import XMonad.Hooks.DynamicProperty
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageHelpers
  ( doCenterFloat,
    isDialog,
  )
import XMonad.Layout.ThreeColumns (ThreeCol (ThreeColMid))
import XMonad.ManageHook ()
import XMonad.StackSet
  ( RationalRect (..),
    allWindows,
    current,
    tag,
    workspace,
  )
import XMonad.Util.EZConfig (mkKeymap)
import XMonad.Util.NamedScratchpad
  ( NamedScratchpad (NS),
    customFloating,
    namedScratchpadAction,
    namedScratchpadManageHook,
  )
import Prelude hiding (pred)

main :: IO ()
main =
  xmonad $
    ewmh $
      def
        { terminal = "alacritty",
          modMask = mod4Mask,
          borderWidth = 1,
          manageHook =
            composeAll
              [isDialog --> doFloat, namedScratchpadManageHook scratchpads],
          workspaces = ["hme", "wrk", "3", "4", "5", "6", "7", "8", "9"],
          layoutHook = customLayout,
          -- Spotify annoyingly does not set its window property on startup but
          -- rather later. This is why we have to listen for a dynamic property
          -- change and catch this specifically for this application.
          handleEventHook = dynamicPropertyChange "WM_NAME" (title =? "Spotify" --> customFloating (RationalRect 0 0 (1 / 4) (2 / 3))),
          keys = keyConf
        }
  where
    customLayout = ThreeColMid 1 (3 / 100) (1 / 2) ||| Full

scratchpads :: [NamedScratchpad]
scratchpads =
  [ NS "media" spawnMedia findMedia layoutMedia,
    NS "search" spawnSearch findSearch layoutSearch,
    NS "term" spawnScratch findScratch layoutScratch,
    NS "sound" spawnSound findSound layoutSound,
    NS "trade" spawnTrade findTrade layoutTrade
  ]
  where
    spawnMedia = "spotify"
    findMedia = className =? "Spotify"
    layoutMedia =
      customFloating (RationalRect 0 0 (1 / 4) (2 / 3))
    spawnSearch = "firefox -P search --class=\"SEARCH\""
    findSearch = className =? "SEARCH"
    layoutSearch =
      doCenterFloat <+> customFloating (RationalRect 0 0 (2 / 3) (2 / 3))
    spawnScratch =
      "alacritty --class SCRATCH,SCRATCH -e env TERM=screen-256color tmux new-session -s scratch"
    findScratch = className =? "SCRATCH"
    layoutScratch = customFloating (centeredRationalRect (2 / 3) (1 / 2))
    spawnSound = "pavucontrol"
    findSound = className =? "Pavucontrol"
    layoutSound =
      doCenterFloat <+> customFloating (RationalRect 0 0 (1 / 4) (1 / 2))
    spawnTrade = "firefox -P trade --class=\"TRADE\""
    findTrade = className =? "TRADE"
    layoutTrade =
      doCenterFloat <+> customFloating (RationalRect 0 0 (2 / 3) (2 / 3))

-- Some X windows are getting resized when being moved by `doFloat...`
-- functions. So instead of customFloating a window and composing the
-- `ManageHook`s, one can use this function to spawn a centered window of the
-- given size.
centeredRationalRect :: Rational -> Rational -> RationalRect
centeredRationalRect width height = RationalRect x y width height
  where
    x = (1 / 2) - width / 2
    y = (1 / 2) - height / 2

keyConf :: XConfig Layout -> M.Map (ButtonMask, KeySym) (X ())
keyConf c = keyMap c `M.union` keys def c
  where
    keyMap = \c ->
      mkKeymap
        c
        [ ("M-p", spawn "$HOME/.config/rofi/launchers/text/launcher.sh"),
          ("M-S-s", spawn "$HOME/.config/rofi/applets/menu/screenshot.sh"),
          ( "M-b",
            doOnWorkspace
              [ ("hme", spawn "firefox -P private --class=\"private\""),
                ("wrk", spawn "firefox -P work --class=\"work\"")
              ]
          ),
          ( "M-S-<Return>",
            doOnPredicate
              [ ( className =? "obs",
                  spawn "alacritty"
                ),
                (elseDo, spawn $ terminal c)
              ]
          ),
          ("M-d", spawn "firejail discord"),
          ("M-+", spawn "pamixer -i 5"),
          ("M--", spawn "pamixer -d 5"),
          ("M-=", spawn "pamixer --set-volume 100"),
          ("M-S-m", spawn "pamixer -t"),
          ("M-q", spawn "xmonad --recompile && xmonad --restart"),
          ("M-x", namedScratchpadAction scratchpads "media"),
          ("M-s", namedScratchpadAction scratchpads "search"),
          ("M-S-g", namedScratchpadAction scratchpads "trade"),
          ("M-u", namedScratchpadAction scratchpads "term"),
          ("M-S-a", namedScratchpadAction scratchpads "sound"),
          ("XF86MonBrightnessDown", spawn "light -U 5"),
          ("XF86MonBrightnessUp", spawn "light -A 5")
        ]

-- Convenience function.
elseDo :: Query Bool
elseDo = return True

-- doOnWorkspace compares the given list of `WorkspaceId`s to the current
-- workspace and executes the related action. Applies the first matching action
-- else nothing.
doOnWorkspace :: [(WorkspaceId, X ())] -> X ()
doOnWorkspace preds = gets windowset >>= go preds . tag . workspace . current
  where
    go :: [(WorkspaceId, X ())] -> WorkspaceId -> X ()
    go [] _ = return ()
    go ((ws, action) : ps) wid
      | ws == wid = action
      | otherwise = go ps wid

-- doOnPredicate receives a list of `(Query, Action)` tuples and applies the
-- first matching action for which a `Query` returns `True`.
doOnPredicate :: [(Query Bool, X ())] -> X ()
doOnPredicate [] = return ()
doOnPredicate ((pred, action) : ps) = do
  s <- get >>= \s -> return $ windowset s
  filterAll <- filterM (runQuery pred) (allWindows s)
  if filterAll /= [] then action else doOnPredicate ps
