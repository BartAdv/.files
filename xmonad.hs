import XMonad
import XMonad.Actions.SpawnOn
import XMonad.Actions.Submap
import XMonad.Actions.CycleWS
import XMonad.Layout.IndependentScreens
import XMonad.Wallpaper
import XMonad.Util.EZConfig

import Control.Arrow
import Control.Monad.Fix (fix)
import Data.Bits
import Data.Maybe (fromMaybe)
import qualified XMonad.StackSet as W
import qualified Data.Map as Map

myWorkspaces = ["emacs", "web", "terminals"] ++ map show [4..9]

myKeys = [("M-d", nextScreen)
         ,("M-p", spawn "dmenu_run -fn 'Source Code Pro-18'")
         ,("M1-<Tab>", windows W.focusDown)
         ,("M1-S-<Tab>", windows W.focusUp)
         ,("<XF86MonBrightnessDown>", spawn "xbacklight -dec 10")
         ,("<XF86MonBrightnessUp>", spawn "xbacklight -inc 10")
         ,("<XF86AudioLowerVolume>", spawn "amixer set Master 5-")
         ,("<XF86AudioRaiseVolume>", spawn "amixer set Master 5+")
         ,("<XF86AudioMute>", spawn "amixer set Master toggle")]

main = do
  setRandomWallpaper ["$HOME/Wallpapers"]
  let allKeys = keys $ additionalKeysP def myKeys -- normal, chord keybindings
      seqKeys = addPrefix (0, xK_Super_L) allKeys -- sequences
  xmonad $ def
    { modMask = mod4Mask
    , borderWidth = 0
    , workspaces = withScreens 2 myWorkspaces
    , keys = \conf -> Map.union (allKeys conf) (seqKeys conf)
    , startupHook = spawn "compton -bf --inactive-dim 0.4 -D 4"}

-- http://kojevnikov.com/xmonad-metacity-gnome.html
addPrefix p ms conf =
  Map.singleton p . submap (ms conf) $ Map.mapKeys (first chopMod) (ms conf)
  where
    mod = modMask conf
    chopMod = (.&. complement mod)
    -- just like submap, but default action is to grab action from provided keymap
    submap :: Map.Map (KeyMask, KeySym) (X ()) -> Map.Map (KeyMask, KeySym) (X ()) -> X ()
    submap def keys = do
    XConf { theRoot = root, display = d } <- ask

    io $ grabKeyboard d root False grabModeAsync grabModeAsync currentTime

    (m, s) <- io $ allocaXEvent $ \p -> fix $ \nextkey -> do
        maskEvent d keyPressMask p
        KeyEvent { ev_keycode = code, ev_state = m } <- getEvent p
        keysym <- keycodeToKeysym d code 0
        if isModifierKey keysym
            then nextkey
            else return (m, keysym)
    -- Remove num lock mask and Xkb group state bits
    m' <- cleanMask $ m .&. ((1 `shiftL` 12) - 1)

    io $ ungrabKeyboard d currentTime

    let defAction = fromMaybe (return ()) $ Map.lookup (m', s) def
    fromMaybe defAction $ Map.lookup (m', s) keys
