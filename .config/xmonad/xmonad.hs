import qualified Data.Map as M
import XMonad
import XMonad.Util.EZConfig
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.NoBorders
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.UrgencyHook
import XMonad.Prelude
import qualified XMonad.StackSet as S
import XMonad.Util.Loggers
import XMonad.Util.SpawnOnce
import Control.DeepSeq

main :: IO ()
main = xmonad
       . ewmhFullscreen
       . ewmh
       . docks
       $ myConfig


myLog :: X ()
myLog = centerLog >> leftLog
  where
    centerLog = logScreen 0 >>= xmonadPropLog' "_XMONAD_LOG_1"
    leftLog = logScreen 1 >>= xmonadPropLog' "_XMONAD_LOG_2"

logScreen :: ScreenId -> X String
logScreen n = userCodeDef "..." $ do
  winset <- gets windowset
  urgents <- readUrgents
  sort' <- ppSort def

  -- layout description
  ld <- logLayoutOnScreen n

  -- workspace list
  let ws = pprWindowSet sort' urgents xmobarPP winset

  -- run extra loggers, ignoring any that generate errors.
  extras <- mapM (userCodeDef Nothing) $ []

  -- window title
  wt <- logTitleOnScreen n

  return $! force
    $ intercalate (ppSep def)
    . filter (not . null)
    . ppOrder def
    $ [ ws
    , maybe "Unknown" (ppLayout def) ld
    , maybe "" (ppTitle def . ppTitleSanitize def) wt ]
    ++ catMaybes extras

myLayout = smartBorders $ tiled ||| Mirror tiled ||| noBorders Full
  where
    tiled = avoidStruts $ Tall nmaster delta ratio
    nmaster = 1
    ratio = 1/2
    delta = 3/100

myConfig = def
  { modMask = mod4Mask
  , borderWidth = 2
  , focusedBorderColor = "darkgreen"
  , normalBorderColor = "gray"
  , terminal = "kitty"
  , layoutHook = myLayout
  , logHook = myLog
  , startupHook = startupHook def
    >> spawnOnce "kitty emacs --daemon"
    >> killAllStatusBars
    >> spawnStatusBar "xmobar -x 0"
    >> spawnStatusBar "xmobar -x 1 ~/.config/xmobar/left.xmobarrc" }
  `additionalKeysP`
    [ ("M-d", spawn "dmenu_run")
    , ("M-w", spawn "librewolf")
    , ("M-e", spawn "emacsclient -c")
    , ("M-S-e", spawn "emacsclient --eval \"(emacs-everywhere)\"")
    , ("M-<Return>", spawn "kitty")
    , ("M-`", spawn "dmenuunicode")
    , ("M-t", sendMessage $ JumpToLayout "Tall")
    , ("M-S-t", sendMessage $ JumpToLayout "Mirror Tall")
    , ("M-f", sendMessage $ JumpToLayout "Full")
    , ("M-b", sendMessage ToggleStruts)
    , ("M-o", sendMessage $ IncMasterN 1)
    , ("M-S-o", sendMessage . IncMasterN $ -1)
    , ("M-<Space>", windows S.shiftMaster)
    , ("M-S-<Space>", withFocused toggleFloating)
    , ("M-q", kill)
    , ("M-r", spawn "xmonad --restart") ]

toggleFloating :: Window -> X ()
toggleFloating w = windows (\s -> if M.member w $ S.floating s
                                  then S.sink w s
                                  else S.float w (S.RationalRect (1/3) (1/4) (1/2) (4/5)) s)
