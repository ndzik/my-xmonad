module Main where

import           Control.Monad               (filterM, when)
import           Data.Functor                ((<&>))
import qualified Data.Map                    as M
import XMonad
    ( mod4Mask,
      gets,
      runQuery,
      spawn,
      (|||),
      xmonad,
      (-->),
      (<+>),
      (=?),
      className,
      composeAll,
      doFloat,
      ButtonMask,
      KeySym,
      Default(def),
      MonadState(get),
      Layout,
      Query,
      WorkspaceId,
      X,
      XConfig(modMask, borderWidth, manageHook, workspaces, layoutHook,
              terminal, keys),
      XState(windowset),
      Full(Full) )
import           XMonad.Hooks.ManageHelpers  (Side (..), doCenterFloat,
                                              doSideFloat, isDialog)
import XMonad.Layout.ThreeColumns ( ThreeCol(ThreeColMid) )
import XMonad.ManageHook ()
import           XMonad.StackSet             (RationalRect (..), allWindows,
                                              current, tag, workspace)
import XMonad.Util.EZConfig ( mkKeymap )
import XMonad.Util.NamedScratchpad
    ( customFloating,
      namedScratchpadAction,
      namedScratchpadManageHook,
      NamedScratchpad(NS) )

pathToMainhs :: String
pathToMainhs = "$HOME/projects/git/xmonad-config/app/Main.hs"

main :: IO ()
main = xmonad def
  { terminal    = "st"
  , modMask     = mod4Mask
  , borderWidth = 1
  , manageHook  = composeAll
                    [isDialog --> doFloat, namedScratchpadManageHook scratchpads]
  , workspaces  = ["hme", "wrk", "3", "4", "5", "6", "7", "8", "9"]
  , layoutHook  = customLayout
  , keys        = keyConf
  }
  where customLayout = ThreeColMid 1 (3 / 100) (1 / 2) ||| Full

scratchpads :: [NamedScratchpad]
scratchpads =
  [ NS "media"  spawnMedia   findMedia   layoutMedia
  , NS "search" spawnSearch  findSearch  layoutSearch
  , NS "term"   spawnScratch findScratch layoutScratch
  , NS "sound"  spawnSound   findSound   layoutSound
  , NS "trade" spawnTrade findTrade layoutTrade
  ]
 where
  spawnMedia =
    "firefox -P media --new-tab https://open.spotify.com --class=\"MEDIA\""
  findMedia = className =? "MEDIA"
  layoutMedia =
    doSideFloat NW <+> customFloating (RationalRect 0 0 (1 / 4) (2 / 3))
  spawnSearch = "firefox -P search --class=\"SEARCH\""
  findSearch  = className =? "SEARCH"
  layoutSearch =
    doCenterFloat <+> customFloating (RationalRect 0 0 (2 / 3) (2 / 3))
  spawnScratch  = "st -c SCRATCH -e env TERM=screen-256color tmux"
  findScratch   = className =? "SCRATCH"
  layoutScratch = customFloating (centeredRationalRect (1 / 4) (1 / 2))
  spawnSound    = "pavucontrol"
  findSound     = className =? "Pavucontrol"
  layoutSound =
    doCenterFloat <+> customFloating (RationalRect 0 0 (1 / 4) (1 / 2))
  spawnTrade = "firefox -P trade --class=\"TRADE\""
  findTrade  = className =? "TRADE"
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
  keyMap = \c -> mkKeymap
    c
    [ ("M-p"  , spawn "$HOME/.config/rofi/launchers/text/launcher.sh")
    , ("M-S-s", spawn "$HOME/.config/rofi/applets/menu/screenshot.sh")
    , ( "M-b"
      , doOnWorkspace
        [ ("hme", spawn "firefox -P private --class=\"private\"")
        , ("wrk", spawn "firefox -P work --class=\"work\"")
        ]
      )
    , ("M-S-<Return>", doOnPredicate [(className =? "obs", spawn  "st -f \"VictorMono Nerd Font:pixelsize=20:antialias=true:autohint=true\"")
                                     ,(elseDo, spawn $ terminal c)])
    , ("M-d"  , spawn "firejail discord")
    , ("M-+"  , spawn "pamixer -i 5")
    , ("M--"  , spawn "pamixer -d 5")
    , ("M-="  , spawn "pamixer --set-volume 100")
    , ("M-S-m", spawn "pamixer -t")
    , ( "M-q"
      , spawn
        $ "cat " ++ pathToMainhs ++ "> $HOME/.xmonad/xmonad.hs && xmonad --recompile && xmonad --restart"
      )
    , ("M-x"  , namedScratchpadAction scratchpads "media")
    , ("M-s"  , namedScratchpadAction scratchpads "search")
    , ("M-S-g", namedScratchpadAction scratchpads "trade")
    , ("M-u"  , namedScratchpadAction scratchpads "term")
    , ("M-S-a", namedScratchpadAction scratchpads "sound")
    , ("XF86MonBrightnessDown", spawn "light -U 5")
    , ("XF86MonBrightnessUp", spawn "light -A 5")
    ]

-- Conventience function.
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
  go ((ws, action) : ps) wid | ws == wid = action
                             | otherwise = go ps wid

-- doOnPredicate receives a list of `(Query, Action)` tuples and applies the
-- first matching action for which a `Query` returns `True`.
doOnPredicate :: [(Query Bool, X ())] -> X ()
doOnPredicate [] = return ()
doOnPredicate ((pred, action):ps) = do
      s <- get >>= \s -> return $ windowset s
      filterAll <- filterM (runQuery pred) (allWindows s)
      if filterAll /= [] then action else doOnPredicate ps
