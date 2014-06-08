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

loadResources ∷ Game Resources
loadResources = do
  b ← readBitmap "data/images/backdrop_dark.png"
  s ← readBitmap "data/images/shana.png"
  let standing = cropStanding s
  return $ Resources { _charSprites = (standing , 0, 0)
                     , _backdrop = b }
  where
    cropStanding ∷ Bitmap → V.Vector Bitmap
    cropStanding b =
      let (w, h, wo, ho) = (53, 60, 35, 10)
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
      sp ← use (resources.charSprites) >>= \case
        (v, s, d) → if d < 10
                    then do
                      resources.charSprites .= (v, s, d + 1)
                      return $ v V.! s
                    else if s + 1 >= V.length v
                         then do
                           resources.charSprites .= (v, 0, 0)
                           return $ v V.! s
                         else do
                           resources.charSprites .= (v, s + 1, 0)
                           return $ v V.! s
      -- (v, s) ← use (resources.charSprites)
      -- let sps = zip [1 .. ] (V.toList v)
      -- mapM_ (\(n, b) → translate (V2 (300 + (60 * n)) 300) $ bitmap b) sps

      translate (V2 400 300) $ bitmap sp

      whenM (keyPress KeyEscape) $ quit .= True
      q ← use quit
      tick
      unless q mainloop

    b ∷ BoundingBox2
    b = Box (V2 0 0) (V2 800 600)
