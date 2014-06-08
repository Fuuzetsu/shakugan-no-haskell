{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
module Shakugan.Types where

import Control.Lens
import Control.Monad.State.Strict
import qualified Data.Map as M
import qualified Data.Vector as V
import FreeGame

data Resources =
  Resources { _charSprites ∷ V.Vector Bitmap
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
