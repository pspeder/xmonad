-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Actions.FloatKeys
-- Copyright    : (c) Karsten Schoelzel <kuser@gmx.de>
-- License      : BSD
--
-- Maintainer   : Karsten Schoelzel <kuser@gmx.de>
-- Stability    : stable
-- Portability  : unportable
--
-- Move and resize floating windows.
--
-- This version was downloaded from:
--   https://github.com/wh5a/arch/blob/master/.xmonad/lib/FloatKeys.hs
--
--   It is set to "disrespect" size hints rather than trying (and failing)
--   to respect them (my problem was that the original module kept failing
--   at resizing scratchpads).
--
-----------------------------------------------------------------------------

module XMonad.Actions.FloatKeys (
                -- * Usage
                -- $usage
                keysMoveWindow,
                keysMoveWindowTo,
                keysResizeWindow,
                keysAbsResizeWindow) where

import XMonad
import Control.Arrow ((***))

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- >    import XMonad.Actions.FloatKeys
--
-- Then add appropriate key bindings, for example:
--
-- >  , ((modm,               xK_d     ), withFocused (keysResizeWindow (-10,-10) (1,1)))
-- >  , ((modm,               xK_s     ), withFocused (keysResizeWindow (10,10) (1,1)))
-- >  , ((modm .|. shiftMask, xK_d     ), withFocused (keysAbsResizeWindow (-10,-10) (1024,752)))
-- >  , ((modm .|. shiftMask, xK_s     ), withFocused (keysAbsResizeWindow (10,10) (1024,752)))
-- >  , ((modm,               xK_a     ), withFocused (keysMoveWindowTo (512,384) (1%2,1%2)))
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".

-- | @keysMoveWindow (dx, dy)@ moves the window by @dx@ pixels to the
--   right and @dy@ pixels down.
keysMoveWindow :: D -> Window -> X ()
keysMoveWindow (dx,dy) w = whenX (isClient w) $ withDisplay $ \d -> do
    io $ raiseWindow d w
    wa <- io $ getWindowAttributes d w
    io $ moveWindow d w (fromIntegral (fromIntegral (wa_x wa) + dx))
                        (fromIntegral (fromIntegral (wa_y wa) + dy))
    float w

-- | @keysMoveWindowTo (x, y) (gx, gy)@ moves the window relative
--   point @(gx, gy)@ to the point @(x,y)@, where @(gx,gy)@ gives a
--   position relative to the window border, i.e.  @gx = 0@ is the left
--   border, @gx = 1@ is the right border, @gy = 0@ is the top border, and
--   @gy = 1@ the bottom border.
--
--   For example, on a 1024x768 screen:
--
-- > keysMoveWindowTo (512,384) (1%2, 1%2) -- center the window on screen
-- > keysMoveWindowTo (1024,0) (1, 0)      -- put window in the top right corner
keysMoveWindowTo :: P -> G -> Window -> X ()
keysMoveWindowTo (x,y) (gx, gy) w = whenX (isClient w) $ withDisplay $ \d -> do
    io $ raiseWindow d w
    wa <- io $ getWindowAttributes d w
    io $ moveWindow d w (x - round (gx * fromIntegral (wa_width wa)))
                        (y - round (gy * fromIntegral (wa_height wa)))
    float w

type G = (Rational, Rational)
type P = (Position, Position)

-- | @keysResizeWindow (dx, dy) (gx, gy)@ changes the width by @dx@
--   and the height by @dy@, leaving the window-relative point @(gx,
--   gy)@ fixed.
--
--   For example:
--
-- > keysResizeWindow (10, 0) (0, 0)      -- make the window 10 pixels larger to the right
-- > keysResizeWindow (10, 0) (0, 1%2)    -- does the same, unless sizeHints are applied
-- > keysResizeWindow (10, 10) (1%2, 1%2) -- add 5 pixels on each side
-- > keysResizeWindow (-10, -10) (0, 1)   -- shrink the window in direction of the bottom-left corner
keysResizeWindow :: D -> G -> Window -> X ()
keysResizeWindow = keysMoveResize keysResizeWindow'

-- | @keysAbsResizeWindow (dx, dy) (ax, ay)@ changes the width by @dx@
--   and the height by @dy@, leaving the screen absolute point @(ax,
--   ay)@ fixed.
--
--   For example:
--
-- > keysAbsResizeWindow (10, 10) (0, 0)   -- enlarge the window; if it is not in the top-left corner it will also be moved down and to the right.
keysAbsResizeWindow :: D -> D -> Window -> X ()
keysAbsResizeWindow = keysMoveResize keysAbsResizeWindow'

keysAbsResizeWindow' :: SizeHints -> P -> D -> D -> D -> (P,D)
keysAbsResizeWindow' sh (x,y) (w,h) (dx,dy) (ax, ay) = ((round nx, round ny), (nw, nh))
    where
        (minw, minh) = maybe (10,10) ((***) (max 10) (max 10)) $ sh_min_size sh
        (nw, nh) = if w + dx > minw && h + dy > minh then applySizeHintsContents sh (w + dx, h + dy)
                   else (w, h)
        nx :: Rational
        nx = fromIntegral (ax * w + nw * (fromIntegral x - ax)) / fromIntegral w
        ny :: Rational
        ny = fromIntegral (ay * h + nh * (fromIntegral y - ay)) / fromIntegral h

keysResizeWindow' :: SizeHints -> P -> D -> D -> G -> (P,D)
keysResizeWindow' sh (x,y) (w,h) (dx,dy) (gx, gy) = ((nx, ny), (nw, nh))
    where
        -- Prevent shrinking a window too small and getting one's foot shot
        (minw, minh) = maybe (10,10) ((***) (max 10) (max 10)) $ sh_min_size sh
        (nw, nh) = if w + dx > minw && h + dy > minh then applySizeHintsContents sh (w + dx, h + dy)
                   else (w, h)
        nx = round $ fromIntegral x + gx * fromIntegral w - gx * fromIntegral nw
        ny = round $ fromIntegral y + gy * fromIntegral h - gy * fromIntegral nh

keysMoveResize :: (SizeHints -> P -> D -> a -> b -> (P,D)) -> a -> b -> Window -> X ()
keysMoveResize f move resize w = whenX (isClient w) $ withDisplay $ \d -> do
    io $ raiseWindow d w
    wa <- io $ getWindowAttributes d w
    sh' <- io $ getWMNormalHints d w
    let sh = sh' {sh_resize_inc = Nothing, sh_aspect = Nothing}
        wa_dim = (fromIntegral $ wa_width wa, fromIntegral $ wa_height wa)
        wa_pos = (fromIntegral $ wa_x wa, fromIntegral $ wa_y wa)
        (wn_pos, wn_dim) = f sh wa_pos wa_dim move resize
    io $ resizeWindow d w `uncurry` wn_dim
    io $ moveWindow d w `uncurry` wn_pos
    float w
