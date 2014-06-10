{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
module Main where

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
  let tf = 60
  setFPS tf
  setTitle "shakugan-no-haskell"
  clearColor $ Color 0 0 0 0
  r ← loadResources
  let mb = r ^. charSprites.charFacingRight.spriteMaps ^?! ix 0 ^. movingBitmap
  let field' = Field { _player = Player
                                   { _keysHeld = M.empty
                                   , _position = V2 100 500
                                   , _facing = RightD
                                   , _falling = False
                                   , _jumping = False
                                   , _lastUsed = mb
                                   }
                     }

  evalStateT mainloop GameFrame { _resources = r
                                , _field = field'
                                , _quit = False
                                , _targetFramerate = tf
                                }
  where
    mainloop ∷ GameLoop ()
    mainloop = do
      bd ← use (resources.backdrop)
      let (w, h) = bitmapSize bd & both %~ ((/ 2) . fromIntegral)
          _cs = animate 2 charSprites

      translate (V2 w h) $ bitmap bd

      pos ← use (field.player.position)
      con ← pressedKeys >>= characterControl
      translate pos $ bitmap con
      -- drawSeries True (resources.charSprites.charJumpingRight)

      whenM (keyPress KeyEscape) $ quit .= True
      whenM (keyPress KeyO) $ field.player.position .= V2 100 500

      q ← use quit
      tick
      unless q mainloop

    b ∷ BoundingBox2
    b = Box (V2 0 0) (V2 800 600)
