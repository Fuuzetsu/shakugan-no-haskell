{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
module Shakugan.Types where

import Control.Lens
import Control.Monad.State.Strict
import qualified Data.Map as M
import qualified Data.Vector as V
import FreeGame


makeLenses ''V.Vector

-- | Collection of bitmaps with some meta-data to keep track of where
-- we are.
data Sprite =
  Sprite { _spriteMaps ∷ V.Vector Bitmap -- ^ Vector of sprites for this graphic
         , _spriteNext ∷ Int -- ^ Index of next sprite to render
         , _spriteSince ∷ Int -- ^ Frames since sprite was changed
         }

makeLenses ''Sprite

data Effects = Effects { _effectFireball ∷ Sprite
                       , _effectFirebeam ∷ Sprite
                       }

makeLenses ''Effects

data CharacterSprites =
  CharacterSprites { _charFacingLeft ∷ Sprite
                   , _charFacingRight ∷ Sprite
                   , _charRunningLeft ∷ Sprite
                   , _charRunningRight ∷ Sprite
                   , _effects ∷ Effects
                   }

makeLenses ''CharacterSprites

data Resources =
  Resources { _charSprites ∷ CharacterSprites
            , _backdrop ∷ Bitmap
            }

makeLenses ''Resources

data Player = Player { _keysHeld ∷ M.Map Key Int }

makeLenses ''Player

data Field =
  Field { _player ∷ Player }

makeLenses ''Field

data GameFrame =
  GameFrame { _resources ∷ Resources
            , _field ∷ Field
            , _quit ∷ Bool }

makeLenses ''GameFrame

type GameLoop = StateT GameFrame Game
