{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- module      :  Data.GF256.GenPoly256
-- Copyright   :  (c) 2017 Naoyuki MORITA
-- License     :  MIT
--
-- Maintainer  :  naoyuki.morita@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (TemplateHaskell)
--
-----------------------------------------------------------------------------

module Data.GF256.GenPoly256
  (GenPoly256 (..)
  -- * Template haskell utilities
  ,declInstGP256
  ) where

import qualified Language.Haskell.TH as TH
import TypeLevel.Number.Nat
import Data.Bits
import qualified Data.Word as W
import Data.List (sort)
import qualified Data.Vector.Unboxed as V

-- | Type class for generator polynomial of \(\mathrm{GF}(2^8)\)
class GenPoly256 k where
  genVal :: k
  genInt :: k -> Int
  logTable :: k -> V.Vector Int
  powTable :: k -> V.Vector W.Word8

genPowList :: Int -> [W.Word8]
genPowList x = map fromIntegral $ take 255 $ iterate f 1
  where f z | (z .&. 128) /= 0  = (z `shiftL` 1) `xor` x
            | otherwise         = (z `shiftL` 1)

genPowTable :: Int -> V.Vector W.Word8
genPowTable x = V.fromList $ genPowList x

genLogTable :: Int -> V.Vector Int
genLogTable x = V.fromList $ map snd $ sort $ zip (genPowList x) [0..254]

patUnbox :: TH.Name -> String -> TH.PatQ
patUnbox cname vname = TH.conP cname [TH.varP =<< TH.newName vname]

noBang :: TH.Q TH.Bang
noBang = TH.bang TH.noSourceUnpackedness TH.noSourceStrictness

declInstGP256' :: TH.Name -> TH.Name -> TH.DecsQ
declInstGP256' nameT nameV =
  [d|
    instance GenPoly256 $(TH.conT nameT) where
      genVal = $(TH.conE nameV) undefined
      genInt $(patUnbox nameV "x") = toInt x
      logTable x = genLogTable $ genInt x
      powTable x = genPowTable $ genInt x
   |]

declNT :: TH.Name -> TH.Name -> Integer -> TH.DecQ
declNT tcon vcon n = TH.newtypeD cxt tcon [] Nothing (body vcon) drv
  where cxt = TH.cxt []
        tp  = TH.bangType noBang (natT n)
        body nm = TH.normalC nm [tp]
        drv = sequence []

-- | Template function that generates top-level declaration of
--  primitive polynomial.
declInstGP256 :: Integer -> TH.DecsQ
declInstGP256 n = do
  tcon <- TH.newName $ "PP" ++ show n
  vcon <- TH.newName $ "PP" ++ show n
  ntdec <- declNT tcon vcon n
  instds <- declInstGP256' tcon vcon
  return $ ntdec : instds

-- EOF
