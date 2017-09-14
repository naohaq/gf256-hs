{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}
{-# LANGUAGE CPP, KindSignatures, DataKinds, ConstraintKinds #-}
{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- module      :  Data.GF256
-- Copyright   :  (c) 2017 Naoyuki MORITA
-- License     :  MIT
--
-- Maintainer  :  naoyuki.morita@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (ScopedTypeVariables, DeriveDataTypeable)
--
-----------------------------------------------------------------------------

module Data.GF256
  ( GF256
  , toInteger
  , module Data.GF256.GenPoly256
  ) where

import Prelude hiding (toInteger)
import qualified Data.GF2 as F2
-- import Data.FiniteField.Base
import Data.ExtensionF2
import qualified Data.Array.Unboxed as UA
import Data.Ratio
import Data.Bits
import Data.Typeable

import Data.GF256.GenPoly256

newtype GF256 a = GF256 Int deriving (Eq, Typeable)

toInteger :: GF256 a -> Integer
toInteger (GF256 x) = fromIntegral x

genPrimList :: (GenPoly256 a) => a -> [Int]
genPrimList x = 1 : (takeWhile (/= 1) $ iterate nextElem 2)
  where gp = genInt x
        nextElem :: Int -> Int
        nextElem y = let z = y * 2 in
                       if (z .&. 256) /= 0
                       then z `xor` gp
                       else z

instance (GenPoly256 a) => ExtensionF2 (GF256 a) where
  generator _ = genInt (undefined :: a)
  fromInt x = ret
    where ret = GF256 $ x `F2.mod` generator ret
  toInt (GF256 x) = x
  primitives _ = genPrimList (undefined :: a)
  log2 x = tbl UA.! (toInt x)
    where tbl = logTable (undefined :: a)
  pow2 x = fromInt $ tbl UA.! (x `mod` 255)
    where tbl = powTable (undefined :: a)
  degree _ = 8

instance Show (GF256 a) where
  showsPrec n (GF256 x) = showsPrec n x

instance (GenPoly256 a) => Num (GF256 a) where
  x + y = fromInt $ toInt x `xor` toInt y
  x * y = pow2 $ (log2 x + log2 y)
  x - y = x + y
  negate x = x
  abs x = x
  signum _ = 1
  fromInteger x = fromInt $ fromIntegral $ x `F2.mod` fromIntegral (genInt (undefined :: a))

instance (GenPoly256 a) => Fractional (GF256 a) where
  fromRational r = fromInteger (numerator r) / fromInteger (denominator r)
  recip x = pow2 $ 255 - log2 x

{-
instance (GenPoly256 a) => FiniteField (GF256 a) where
  order _ = 256
  char  _ = 2
  pthRoot x = x * x
  allValues = 0 : map pow2 [0..254]
-}

-- EOF
