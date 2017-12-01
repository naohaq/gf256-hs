{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- module      :  Data.GF2
-- Copyright   :  (c) 2017 Naoyuki MORITA
-- License     :  MIT
--
-- Maintainer  :  naoyuki.morita@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (DeriveDataTypeable)
--
-----------------------------------------------------------------------------

module Data.GF2
  ( GF2
  , fromGF2
  ) where

import Data.Ratio
import Data.Bits ( xor, (.&.) )
#if defined(InstanceOfFiniteField)
import Data.FiniteField (FiniteField(..))
#endif
import Data.Typeable

newtype GF2 = GF2 Bool deriving (Eq, Typeable)

boolToInteger :: Bool -> Integer
boolToInteger False = 0
boolToInteger True  = 1

fromGF2 :: (Num a) => GF2 -> a
fromGF2 (GF2 x) = fromInteger $ boolToInteger x

instance Show GF2 where
  showsPrec n (GF2 x) = showsPrec n $ boolToInteger x

instance Num GF2 where
  (GF2 x) + (GF2 y) = GF2 $ x `xor` y
  (GF2 x) * (GF2 y) = GF2 $ x .&. y
  x - y = x + y
  negate x = x
  abs    x = x
  signum x = x
  fromInteger x = GF2 $ (x `mod` 2) == 1

instance Fractional GF2 where
  fromRational r = fromInteger (numerator r) / fromInteger (denominator r)
  recip 0 = error "divide by zero"
  recip x = x

instance Enum GF2 where
  toEnum x = GF2 $ toEnum x
  fromEnum (GF2 x) = fromEnum x

instance Ord GF2 where
  compare (GF2 x) (GF2 y) = compare x y

instance Bounded GF2 where
  minBound = 0
  maxBound = 1

#if defined(InstanceOfFiniteField)
instance FiniteField GF2 where
  order _ = 2
  char  _ = 2
  pthRoot x = x
  allValues = [0,1]
#endif

-- EOF
