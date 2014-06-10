{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
module Shakugan.CharacterControl where

import           Control.Lens
import           Data.List (sortBy)
import qualified Data.Vector as V
import           FreeGame
import           Shakugan.Types
import           Shakugan.Util

-- | Animate whole sprite specified by the lens twice a second.
animateChar ∷ Lens' CharacterSprites Sprite → GameLoop Bitmap
animateChar = animate 3 charSprites

-- | If we're jumping already, keep jumping in the same direciton.
keepJumping ∷ GameLoop Bitmap
keepJumping = use (field.player.facing) >>= \case
  LeftD → animateChar charJumpingLeft
  RightD → animateChar charJumpingRight

-- | Process list of currently pressed keys and return next sprite of
-- character animation.
characterControl ∷ [Key] → GameLoop Bitmap
characterControl x = use (field.player.jumping) >>= \case
  True → do
    Sprite _ ir sr ← use (c.charJumpingRight)
    Sprite _ il sl ← use (c.charJumpingLeft)
    if ir == 0 && sr == 0 && il == 0 && sl == 0
      then do field.player.jumping .= False >> characterControl x
      else keepJumping
  False → case sortBy keyPriorities x of
    []         → use (field.player.facing) >>= \case
      LeftD  → animateChar charFacingLeft
      RightD → animateChar charFacingRight
    KeyUp:r    → use (field.player.facing) >>= \d → do
      field.player.jumping .= True

      -- Skip first frame if we're moving, running start.
      let moving = KeyLeft `elem` r || KeyRight `elem` r
      case d of
        LeftD  → do
          when moving (skipFrame $ c.charJumpingLeft)
          animateChar charJumpingLeft
        RightD → do
          when moving (skipFrame $ c.charJumpingRight)
          animateChar charJumpingRight
    KeyLeft:_  → do
      field.player.facing .= LeftD
      animateChar charRunningLeft
    KeyRight:_ → do
      field.player.facing .= RightD
      animateChar charRunningRight
    _:xs → characterControl xs
  where
    c ∷ Lens' GameFrame CharacterSprites
    c = resources.charSprites

jumpSprite ∷ Direction → Lens' GameFrame Sprite
jumpSprite LeftD = resources.charSprites.charJumpingLeft
jumpSprite RightD = resources.charSprites.charJumpingRight

-- | Priorities certain keys in favour of others.
keyPriorities ∷ Key → Key → Ordering
keyPriorities KeyUp _ = LT
keyPriorities _ KeyUp = GT
keyPriorities x y = compare x y

-- | Skips frame of animation if it's not the last one already.
skipFrame ∷ Lens' GameFrame Sprite → GameLoop ()
skipFrame f = use f >>= \(Sprite m n _) →
  if n + 1 >= V.length m
  then return ()
  else f .= Sprite m (n + 1) 0
