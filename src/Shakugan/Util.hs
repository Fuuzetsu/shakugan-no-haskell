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
      (resources.charSprites .=) $ cs & g .~
        if d < floor (60 / t / fromIntegral (V.length v))
        then Sprite v s (d + 1)
        else if s + 1 >= V.length v
             then Sprite v 0 0
             else Sprite v (s + 1) 0

      -- Don't want to change player position at each rendered frame,
      -- just each individual sprite change.
      if d == 0
        then runBitmap $ v V.! s
        else return $ v ^?! ix s . movingBitmap


-- | Updates player position based by how much the current sprite
-- dictates and returns the underlying 'Bitmap'.
runBitmap ∷ MovingBitmap → GameLoop Bitmap
runBitmap (MovingBitmap b pd) = field.player.position %= (^+^ pd) >> return b

pressedKeys ∷ GameLoop [Key]
pressedKeys = M.keys . M.filter id <$> keyStates
