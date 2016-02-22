-- xmonad.hs
-- Main configuration file for Paw Saabye's XMonad setup.
import XMonad               ( xmonad )
import XMonad.Config.Psp    ( pspConfig )
import XMonad.Util.Run      ( spawnPipe )
import XMonad.Util.Replace  ( replace )
import System.Process

main :: IO()
main = do
    replace -- instruct current (if any) window manager to exit
    xmonad $ pspConfig

