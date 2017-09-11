{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}
{-# LANGUAGE CPP, KindSignatures, DataKinds, ConstraintKinds #-}
{-# OPTIONS_GHC -Wall #-}

module Data.GF256
  ( GF256
  , toInteger
  , module Data.GF256.GenPoly256
  ) where

import Prelude hiding (toInteger)
import qualified Data.GF2 as F2
import Data.ExtendedF2
import Data.List (sort)
import qualified Data.Array.Unboxed as UA
import Data.Ratio
import Data.Bits
import Data.Typeable

import Data.GF256.GenPoly256

newtype GF256 a = GF256 Int deriving (Eq, Typeable)

toInteger :: GF256 a -> Integer
toInteger (GF256 x) = fromIntegral x

newtype PrimitiveArray = PA (UA.UArray Int Int)
newtype LogArray = LA (UA.UArray Int Int)

genPrimList :: (GenPoly256 a) => a -> [Int]
genPrimList x = 1 : (takeWhile (/= 1) $ iterate nextElem 2)
  where gp = genInt x
        nextElem :: Int -> Int
        nextElem y = let z = y * 2 in
                       if (z .&. 256) /= 0
                       then z `xor` gp
                       else z

genLogList :: (GenPoly256 a) => a -> [(Int,Int)]
genLogList x = sort $ zip (genPrimList x) [0..]

primTable :: (GenPoly256 a) => a -> PrimitiveArray
primTable x = PA $ UA.array (0,254) $ zip [0..254] $ genPrimList x

logTable :: (GenPoly256 a) => a -> LogArray
logTable x = LA $ UA.array (1,255) $ genLogList x

instance (GenPoly256 a) => ExtendedF2 (GF256 a) where
  generator _ = genInt (undefined :: a)
  fromInt x = ret
    where ret = GF256 $ x `F2.mod` generator ret
  toInt (GF256 x) = x
  primitives _ = genPrimList (undefined :: a)
  log2 x = tbl UA.! (toInt x)
    where LA tbl = logTable (undefined :: a)
  pow2 x = fromInt $ tbl UA.! (x `mod` 255)
    where PA tbl = primTable (undefined :: a)

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

