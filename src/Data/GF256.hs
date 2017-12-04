{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- module      :  Data.GF256
-- Copyright   :  (c) 2017 Naoyuki MORITA
-- License     :  MIT
--
-- Maintainer  :  naoyuki.morita@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (ScopedTypeVariables, DeriveDataTypeable, TemplateHaskell)
--
-----------------------------------------------------------------------------

module Data.GF256
  ( GF256
  , toInteger
  , module Data.GF256.GP256Insts
  , fromWord8
  , toWord8
  ) where

import Prelude hiding (toInteger)
import qualified Data.GF2Polynomial as F2
#if defined(InstanceOfFiniteField)
import Data.FiniteField (FiniteField(..))
#endif
import Data.GF2Extension
import Data.Vector.Unboxed ((!))
import Data.Ratio
import qualified Data.Word as W
import Data.Bits
import Data.Typeable

import Data.GF256.GenPoly256
import Data.GF256.GP256Insts

-- | \(\mathrm{GF}(2^8)\): an extension field of \(\mathrm{GF}(2)\) whose extension degree is 8.
--
-- The generator polynomial must be given as a type parameter.
newtype GF256 a = GF256 W.Word8 deriving (Eq, Typeable)

-- | Conversion to 'Integer'
toInteger :: GF256 a -> Integer
toInteger (GF256 x) = fromIntegral x

-- | Conversion from 'Word8'
fromWord8 :: W.Word8 -> GF256 a
fromWord8 x = GF256 x

-- | Conversion to 'Word8'
toWord8 :: GF256 a -> W.Word8
toWord8 (GF256 x) = x

instance (GenPoly256 a) => GF2Extension (GF256 a) where
  generator _ = genInt (genVal :: a)
  fromInt x = ret
    where ret = GF256 $ fromIntegral $ x `F2.mod` generator ret
  toInt (GF256 x) = fromIntegral x
  log2 (GF256 0) = error "log(0) is undefined"
  log2 (GF256 x) = tbl ! fromIntegral (x - 1)
    where tbl = logTable (genVal :: a)
  pow2 x = ret
    where tbl = powTable (genVal :: a)
          ret = GF256 $ tbl ! (x `mod` 255)
  degree _ = 8

instance Show (GF256 a) where
  showsPrec n (GF256 x) = showsPrec n x

instance (GenPoly256 a) => Num (GF256 a) where
  (GF256 x) + (GF256 y) = GF256 $ x `xor` y
  0 * _ = 0
  _ * 0 = 0
  x * y = pow2 $ (log2 x + log2 y)
  x - y = x + y
  negate x = x
  abs x = x
  signum _ = 1
  fromInteger x = fromWord8 $ fromIntegral $ x `F2.mod` fromIntegral (genInt (genVal :: a))

instance (GenPoly256 a) => Fractional (GF256 a) where
  fromRational r = fromInteger (numerator r) / fromInteger (denominator r)
  recip 0 = error "divide by zero"
  recip x = pow2 $ 255 - log2 x

instance Ord (GF256 a) where
  compare x y = compare (toWord8 x) (toWord8 y)

instance (GenPoly256 a) => Bounded (GF256 a) where
  minBound = 0
  maxBound = 255

instance (GenPoly256 a) => Bits (GF256 a) where
  (GF256 x) .&. (GF256 y) = fromWord8 $ x .&. y
  (GF256 x) .|. (GF256 y) = fromWord8 $ x .|. y
  xor (GF256 x) (GF256 y) = fromWord8 $ xor x y
  complement (GF256 x)    = fromWord8 $ complement x
  shift  (GF256 x) k      = fromWord8 $ shift x k
  rotate (GF256 x) k      = fromWord8 $ rotate x k
  bitSize (GF256 x)       = finiteBitSize x
  bitSizeMaybe (GF256 x)  = bitSizeMaybe x
  isSigned _              = False
  testBit (GF256 x) k     = testBit x k
  bit k                   = fromWord8 $ bit k
  popCount (GF256 x)      = popCount x

instance (GenPoly256 a) => FiniteBits (GF256 a) where
  finiteBitSize (GF256 x) = finiteBitSize x

#if defined(InstanceOfFiniteField)
sqrt' :: (GenPoly256 a) => GF256 a -> GF256 a
sqrt' 0 = 0
sqrt' x = pow2 k
  where n = log2 x
        k = (128 * n) `mod` 255

instance (GenPoly256 a) => FiniteField (GF256 a) where
  order _ = 256
  char  _ = 2
  pthRoot x = sqrt' x
  allValues = 0 : map pow2 [0..254]
#endif

-- EOF
