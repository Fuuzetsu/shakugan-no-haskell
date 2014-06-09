{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Arrow
import Control.Lens
import Control.Monad.State.Strict
import qualified Data.Map as M
import FreeGame
import Shakugan.Types
import Shakugan.Util
import Shakugan.Load
import Shakugan.CharacterControl

main ∷ IO ()
main = void $ runGame Windowed b $ do
  setFPS 60
  setTitle "shakugan-no-haskell"
  clearColor $ Color 0 0 0 0
  r ← loadResources
  let field' = Field { _player = Player
                                   { _keysHeld = M.empty
                                   , _position = V2 100 500
                                   , _facing = RightD
                                   , _falling = False
                                   , _jumping = False
                                   }
                     }

  evalStateT mainloop GameFrame { _resources = r
                                , _field = field'
                                , _quit = False
                                }
  where
    mainloop ∷ GameLoop ()
    mainloop = do
      bd ← use (resources.backdrop)
      let (w, h) = bitmapSize bd & both %~ ((/ 2) . fromIntegral)
          _cs = animate 2 charSprites

      translate (V2 w h) $ bitmap bd

      con ← pressedKeys >>= characterControl
      V2 px py ← use (field.player.position)

      translate (V2 (fromIntegral px) (fromIntegral py)) $ bitmap con

      whenM (keyPress KeyEscape) $ quit .= True
      q ← use quit
      tick
      unless q mainloop

    b ∷ BoundingBox2
    b = Box (V2 0 0) (V2 800 600)
