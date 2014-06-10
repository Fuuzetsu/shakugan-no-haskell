{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
module Shakugan.Util where

import Control.Lens
import qualified Data.Map as M
import qualified Data.Vector as V
import FreeGame
import FreeGame.Class (keyStates)
import Shakugan.Types

-- | Gets next sprite in the animation, bumping the meta-data.
animate ∷ Double -- ^ How many times a second should the whole thing animate?
        → Getter Resources CharacterSprites
        → Lens' CharacterSprites Sprite
        → GameLoop Bitmap
animate t f g = do
  cs ← use $ resources.f
  case cs ^. g of
    Sprite v s d → do
      -- framerate / t = how many times faster/slower we need to go to play
      -- the whole animation once every 60 rendering frames
      -- framerate / t / length v = slice of time we have for each bitmap
      fps ← fromIntegral <$> use targetFramerate
      let timeForFrame = fps / t / fromIntegral (V.length v)
      (resources.charSprites .=) $ cs & g .~
        if d < floor timeForFrame
        then Sprite v s (d + 1)
        else if s + 1 >= V.length v
             then Sprite v 0 0
             else Sprite v (s + 1) 0

      -- When transitioning between separate sprites of same animation
      -- we want to make sure we compensate for varied height and
      -- offset the position immediatelly otherwise it will look like
      -- the taller sprites are falling under the ground level or the
      -- smaller ones are floating.
      b ← runBitmap (1 / timeForFrame) $ v V.! s
      bl ← use (field.player.lastUsed)
      when (d == 0) $ do
        let (_, h) = bitmapSize b
            (_, h') = bitmapSize bl
            df = fromIntegral (h' - h) / 2
        field.player.position %= (^+^ V2 0 df)
        field.player.lastUsed .= b

      return b


-- | Updates player position based on how many rendering frames it
-- visible and how much we need to have moved by the end of this time.
-- Returns the frame that needs to be currently displayed.
--
-- Moving a little bit every rendered frame rather than moving a
-- big chunk at the start of a single sprite makes movement much
-- smoother.
runBitmap ∷ Double -- ^ How big of a chunk of the position we should use.
          → MovingBitmap -- ^ Bitmap to use
          → GameLoop Bitmap
runBitmap s (MovingBitmap b pd) =
  field.player.position %= (^+^ pd ^* s) >> return b

pressedKeys ∷ GameLoop [Key]
pressedKeys = M.keys . M.filter id <$> keyStates

-- | Draws a series of 'Bitmap's from 'Sprite' either horizontally or
-- vertically including the offsets of the sprites.
drawSeries ∷ Bool -- ^ True = horizontal, False = vertical
           → Lens' GameFrame Sprite
           → GameLoop ()
drawSeries way l = do
  Sprite v _ _ ← use l
  let bms = zip [0 .. ] $ V.toList v
      fv p = if way then V2 (100 + 80 * p) 100 else V2 100 (100 + 110 * p)
      f (x, MovingBitmap b offs) = translate (offs ^+^ fv x) $ bitmap b
  mapM_ f bms
