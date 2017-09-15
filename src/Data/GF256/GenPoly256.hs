{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

module Data.GF256.GenPoly256
  (GenPoly256 (..)
  ,declInstGP256
  ) where

import qualified Language.Haskell.TH as TH
import TypeLevel.Number.Nat
import Data.Bits
import Data.List (sort)
import qualified Data.Array.Unboxed as UA

genPowList :: Int -> [Int]
genPowList x = take 255 $ iterate f 1
  where f z | (z .&. 128) /= 0  = (z `shiftL` 1) `xor` x
            | otherwise         = (z `shiftL` 1)

genPowTable :: Int -> UA.UArray Int Int
genPowTable x = UA.array (0,254) $ zip [0..254] $ genPowList x

genLogTable :: Int -> UA.UArray Int Int
genLogTable x = UA.array (1,255) $ sort $ zip (genPowList x) [0..254]

class GenPoly256 k where
  genVal :: k
  genInt :: k -> Int
  logTable :: k -> UA.UArray Int Int
  powTable :: k -> UA.UArray Int Int

declInstGP256 :: TH.Name -> TH.Name -> TH.DecsQ
declInstGP256 nameT nameV =
  [d|
    instance GenPoly256 $(TH.conT nameT) where
      genVal = $(TH.conE nameV) undefined
      genInt ($(TH.conP nameV [ [p|x|] ])) = toInt x
      logTable x = genLogTable $ genInt x
      powTable x = genPowTable $ genInt x
   |]

-- EOF
