{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Lens
import Control.Monad.State.Strict
import qualified Data.Map as M
import qualified Data.Vector as V
import FreeGame
import Shakugan.Types
import Shakugan.Util

loadResources ∷ Game Resources
loadResources = do
  b ← readBitmap "data/images/backdrop_dark.png"
  s ← readBitmap "data/images/shana.png"
  let standingL = cropStandingLeft s
      standingR = cropStandingRight s
  return $ Resources
             { _charSprites = CharacterSprites
                                { _charFacingLeft = Sprite standingL 0 0
                                , _charFacingRight = Sprite standingR 0 0
                                , _charRunningLeft = Sprite standingL 0 0
                                , _charRunningRight = Sprite standingL 0 0
                                }
             , _backdrop = b
             }
  where
    cropStandingLeft ∷ Bitmap → V.Vector Bitmap
    cropStandingLeft b =
      let (w, h, wo, ho) = (53, 60, 276, 10)
      in V.generate 4 (\d → cropBitmap b (w, h) (wo + (w * d), ho))

    cropStandingRight ∷ Bitmap → V.Vector Bitmap
    cropStandingRight b =
      let (w, h, wo, ho) = (53, 60, 34, 10)
      in V.generate 4 (\d → cropBitmap b (w, h) (wo + (w * d), ho))

main ∷ IO ()
main = void $ runGame Windowed b $ do
  setFPS 60
  setTitle "shakugan-no-haskell"
  clearColor $ Color 0 0 0 0
  r ← loadResources
  let field' = Field { _player = Player M.empty }
  evalStateT mainloop GameFrame { _resources = r
                                , _field = field'
                                , _quit = False
                                }
  where
    mainloop ∷ GameLoop ()
    mainloop = do
      bd ← use (resources.backdrop)
      let (w, h) = bitmapSize bd & both %~ ((/ 2) . fromIntegral)
      translate (V2 w h) $ bitmap bd

      sl ← animate 2 charSprites charFacingLeft
      sr ← animate 2 charSprites charFacingRight
      translate (V2 400 300) $ bitmap sl
      translate (V2 330 300) $ bitmap sr

      whenM (keyPress KeyEscape) $ quit .= True
      q ← use quit
      tick
      unless q mainloop

    b ∷ BoundingBox2
    b = Box (V2 0 0) (V2 800 600)
