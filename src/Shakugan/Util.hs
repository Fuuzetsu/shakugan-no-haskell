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
    Sprite v s d →
      if d < floor (60 / t / fromIntegral (V.length v))
      then do
        resources.charSprites .= (cs & g .~ Sprite v s (d + 1))
        return $ v V.! s
      else
        if s + 1 >= V.length v
        then do
          resources.charSprites .= (cs & g .~ Sprite v 0 0)
          return $ v V.! s
        else do
          resources.charSprites .= (cs & g .~ Sprite v (s + 1) 0)
          return $ v V.! s

pressedKeys ∷ GameLoop [Key]
pressedKeys = M.keys . M.filter id <$> keyStates
