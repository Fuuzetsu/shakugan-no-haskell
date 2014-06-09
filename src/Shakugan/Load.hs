{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
module Shakugan.Load where

import qualified Data.Vector as V
import FreeGame
import Shakugan.Types

loadResources ∷ Game Resources
loadResources = do
  b ← readBitmap "data/images/backdrop_dark.png"
  s ← readBitmap "data/images/shana.png"
  let δ         = 5
      standingL = stillMap $ cropStandingLeft s
      standingR = stillMap $ cropStandingRight s
      runningR  = V.map (\x → MovingBitmap x (V2 δ 0)) $ cropRunningRight s
      runningL  = V.map (\x → MovingBitmap x (V2 (-δ) 0)) $ cropRunningLeft s
      firebeam  = stillMap $ cropFirebeam s
      fireball  = stillMap $ cropFireball s
      jumpR     = stillMap $ cropJumpRight s
      jumpL     = stillMap $ cropJumpLeft s
  return $ Resources
             { _charSprites =
                  CharacterSprites
                    { _charFacingLeft = Sprite standingL 0 0
                    , _charFacingRight = Sprite standingR 0 0
                    , _charRunningLeft = Sprite runningL 0 0
                    , _charRunningRight = Sprite runningR 0 0
                    , _charJumpingRight = Sprite jumpR 0 0
                    , _charJumpingLeft = Sprite jumpL 0 0
                    , _effects = Effects
                                   { _effectFireball = Sprite fireball 0 0
                                   , _effectFirebeam = Sprite firebeam 0 0
                                   }
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

    cropRunningRight ∷ Bitmap → V.Vector Bitmap
    cropRunningRight b =
      let (w, h, wo, ho) = (70, 52, 26, 89)
      in V.generate 8 (\d → cropBitmap b (w, h) (wo + (w * d), ho))

    cropRunningLeft ∷ Bitmap → V.Vector Bitmap
    cropRunningLeft b =
      let (w, h, wo, ho) = (70, 52, 20, 149)
      in V.generate 8 (\d → cropBitmap b (w, h) (wo + (w * d), ho))

    cropFirebeam ∷ Bitmap → V.Vector Bitmap
    cropFirebeam b =
      let crp (w, h, wo, ho) = cropBitmap b (w, h) (wo, ho)
          infixr 8 `c`
          x `c` y = crp x `V.cons` y
          f1 = (94, 71, 16, 1675)
          f2 = (106, 79, 123, 1671)
          f3 = (255, 79, 262, 1671)
          f4 = (253, 79, 518, 1671)
      in f1 `c` f2 `c` f3 `c` f4 `c` V.empty

    cropFireball ∷ Bitmap → V.Vector Bitmap
    cropFireball b =
      let crp (w, h, wo, ho) = cropBitmap b (w, h) (wo, ho)
          infixr 8 `c`
          x `c` y = crp x `V.cons` y
          f1  = (8, 8, 30, 1560)
          f2  = (14, 14, 40, 1557)
          f3  = (23, 23, 57, 1552)
          f4  = (23, 23, 85, 1552)
          f5  = (23, 23, 115, 1552)
          f6  = (40, 31, 146, 1546)
          f7  = (40, 31, 192, 1546)
          f8  = (48, 48, 235, 1538)
          f9  = (64, 64, 287, 1527)
          f10 = (80, 76, 357, 1521)
          f11 = (80, 72, 450, 1524)
          f12 = (78, 70, 550, 1525)
      in f1 `c` f2 `c` f3 `c` f4 `c` f5 `c` f6
         `c` f7 `c` f8 `c`f9 `c` f10 `c` f11 `c` f12 `c` V.empty

    cropJumpRight ∷ Bitmap → V.Vector Bitmap
    cropJumpRight b =
      let crp (w, h, wo, ho) = cropBitmap b (w, h) (wo, ho)
          infixr 8 `c`
          x `c` y = crp x `V.cons` y
          f1  = (56, 66, 33, 725)
          f2  = (49, 66, 97, 725)
          f3  = (49, 66, 150, 725)
          f4  = (58, 96, 206, 695)
          f5  = (74, 96, 277, 695)
          f6  = (60, 96, 359, 695)
          f7  = (61, 98, 428, 693)
          f8  = (63, 100, 493, 691)
          f9  = (73, 72, 560, 719)
          f10 = (56, 49, 639, 742)
      in f1 `c` f2 `c` f3 `c` f4 `c` f5 `c` f6
         `c` f7 `c` f8 `c`f9 `c` f10 `c` V.empty


    cropJumpLeft ∷ Bitmap → V.Vector Bitmap
    cropJumpLeft b =
      let crp (w, h, wo, ho) = cropBitmap b (w, h) (wo, ho)
          infixr 8 `c`
          x `c` y = crp x `V.cons` y
          f1  = (56, 66, 1304, 725)
          f2  = (49, 66, 1247, 725)
          f3  = (49, 66, 1194, 725)
          f4  = (58, 96, 1130, 695)
          f5  = (74, 96, 1043, 695)
          f6  = (60, 96, 974, 695)
          f7  = (61, 98, 905, 693)
          f8  = (63, 100, 838, 691)
          f9  = (73, 72, 760, 719)
          f10 = (56, 49, 698, 742)
      in f1 `c` f2 `c` f3 `c` f4 `c` f5 `c` f6
         `c` f7 `c` f8 `c`f9 `c` f10 `c` V.empty

    stillMap ∷ V.Vector Bitmap → V.Vector MovingBitmap
    stillMap = V.map (\x → MovingBitmap x (V2 0 0))
