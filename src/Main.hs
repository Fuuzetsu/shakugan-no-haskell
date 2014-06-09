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
          cs = animate 2 charSprites

      translate (V2 w h) $ bitmap bd

      -- let actmap ∷ M.Map Key (GameLoop Bitmap)
      --     actmap = M.fromList [ (KeyLeft, cs charRunningLeft)
      --                         , (KeyRight, cs charRunningRight)
      --                         ]

      -- ks ← M.filter id <$> keyStates
      -- con ← case M.toList $ M.intersection actmap ks of
      --   (_, x):_ → x
      --   _ → cs charFacingRight


      sl ← cs charFacingLeft
      sr ← cs charFacingRight
      rl ← cs charRunningLeft
      rr ← cs charRunningRight
      fb ← cs (effects.effectFirebeam)
      fba ← cs (effects.effectFireball)
      sj ← animate 1 charSprites charJumpingRight

      translate (V2 365 100) $ bitmap sj
      translate (V2 400 200) $ bitmap sl
      translate (V2 330 200) $ bitmap sr
      translate (V2 400 300) $ bitmap rl
      translate (V2 330 300) $ bitmap rr
      translate (V2 365 400) $ bitmap fb
      translate (V2 365 500) $ bitmap fba

      -- translate (V2 365 200) $ bitmap con

      whenM (keyPress KeyEscape) $ quit .= True
      q ← use quit
      tick
      unless q mainloop

    b ∷ BoundingBox2
    b = Box (V2 0 0) (V2 800 600)
