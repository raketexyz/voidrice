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
       . withSB (xmobarLeft <> xmobarRight)
       $ myConfig

xmobarLeft = statusBarPropTo
  "_XMONAD_LOG_1" "xmobar -x 1 ~/.config/xmobar/left.xmobarrc"
  (pure $ screenPP 0)
xmobarRight = statusBarPropTo "_XMONAD_LOG_2" "xmobar -x 0" (pure $ screenPP 1)

screenPP n = xmobarPP
  { ppOrder = \(_:_:_:r) -> r
  , ppExtras = [ wsLogger n
               , logLayoutOnScreen n
               , fmap (fmap $ xmobarColor "#0f0" "") $ logTitleOnScreen n
               ]
  }

wsLogger :: ScreenId -> Logger
wsLogger n = do
  winset <- gets windowset
  urgents <- readUrgents

  let cur = maybe "" (xmobarColor "yellow" "" . S.tag . S.workspace)
        $ find ((== n) . S.screen) $ S.screens winset
  let others = map (S.tag . S.workspace) . filter ((/= n) . S.screen)
        $ S.screens winset

  let hidden = map S.tag . filter (isJust . S.stack) $ S.hidden winset

  return . Just . wrap "[" "]"
    $ intercalate " " [concat others, cur, concat hidden]

myLayout = avoidStruts (tiled ||| Mirror tiled) ||| noBorders Full
  where
    tiled = smartBorders $ Tall nmaster delta ratio
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
  , startupHook = startupHook def
    >> spawnOnce "kitty emacs --daemon" }
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
toggleFloating w = windows
  (\s -> if M.member w $ S.floating s
         then S.sink w s
         else S.float w (S.RationalRect (1/3) (1/4) (1/2) (4/5)) s)
