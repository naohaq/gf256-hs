{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- module      :  Data.GF2Polynomial
-- Copyright   :  (c) 2017 Naoyuki MORITA
-- License     :  MIT
--
-- Maintainer  :  naoyuki.morita@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (BangPatterns)
--
-- Arithmetic operations treating integral values as polynomials over GF(2).
--
-- For example, the number @1234@ (@10011010010@ in binary) can be seen as:
--
-- > x^10 + x^7 + x^6 + x^4 + x
--
-- In this view, product of two integers is calculated as polynomial
-- multiplication. e.g. @7@ @\`mul\`@ @6@ would be:
--
-- > (x^2 + x + 1) * (x^2 + x) 
-- >   = x^4 + x^3 + x^3 + x^2 + x^2 + x
-- >   = x^4 + x
-- >   = 18
--
-- References:
--
-- * <http://mathworld.wolfram.com/FiniteField.html Finite Field -- from Wolfram MathWorld>
--
-----------------------------------------------------------------------------

module Data.GF2Polynomial
  ( GF2Polynomial (..)
  , order
  ) where

import Prelude hiding (mod, divMod)
import Data.Bits
import Data.Word

-- | Type class for representing polynomials over GF(2)
class GF2Polynomial k where
  -- |Returns the leftmost position of the bit '1'
  highestBit :: k -> Int
  -- |Multiplies two numbers as polynomials over GF(2).
  mul :: k -> k -> k
  -- |Adds two numbers as polynomials over GF(2).
  --
  -- @(a \`add\` b)@ is equivalent to @(a \`xor\` b)@.
  add :: k -> k -> k
  -- |Subtract a number from another number as polynomials over GF(2).
  --
  -- @(a \`sub\` b)@ is equivalent to @(a \`add\` b)@.
  sub :: k -> k -> k
  sub x y = add x y
  -- |Calculates the quotient and the remainder of A/B.
  --
  -- For given \(A(x)\), \(B(x)\neq{0}\), there exists
  -- the quotient \(Q(x)\) and the remainder \(R(x)\)
  -- such that: \(A(x) = Q(x)\cdot{B}(x) + R(x)\).
  divMod :: k -> k -> (k, k)
  -- |The function returns only quotient part of division.
  div :: k -> k -> k
  div x y = fst $ divMod x y
  -- |The function returns only remainder part of division.
  mod :: k -> k -> k
  mod x y = snd $ divMod x y

instance GF2Polynomial Integer where
  highestBit 0 = 0
  highestBit 1 = 0
  highestBit n = 1 + highestBit (n `shiftR` 1)
  mul    x y = mul_f2 x y
  add    x y = add_f2 x y
  divMod x y = divMod_f2 x y

instance GF2Polynomial Int where
  highestBit x = finiteBitSize x - 1 - countLeadingZeros x
  mul    x y = mul_f2 x y
  add    x y = add_f2 x y
  divMod x y = divMod_f2 x y

instance GF2Polynomial Word where
  highestBit x = finiteBitSize x - 1 - countLeadingZeros x
  mul    x y = mul_f2 x y
  add    x y = add_f2 x y
  divMod x y = divMod_f2 x y

instance GF2Polynomial Word64 where
  highestBit x = finiteBitSize x - 1 - countLeadingZeros x
  mul    x y = mul_f2 x y
  add    x y = add_f2 x y
  divMod x y = divMod_f2 x y

instance GF2Polynomial Word32 where
  highestBit x = finiteBitSize x - 1 - countLeadingZeros x
  mul    x y = mul_f2 x y
  add    x y = add_f2 x y
  divMod x y = divMod_f2 x y

instance GF2Polynomial Word16 where
  highestBit x = finiteBitSize x - 1 - countLeadingZeros x
  mul    x y = mul_f2 x y
  add    x y = add_f2 x y
  divMod x y = divMod_f2 x y

instance GF2Polynomial Word8 where
  highestBit x = finiteBitSize x - 1 - countLeadingZeros x
  mul    x y = mul_f2 x y
  add    x y = add_f2 x y
  divMod x y = divMod_f2 x y

mul_f2 :: (Bits a, Num a) => a -> a -> a
mul_f2 x y = iter 0 x y
  where iter !prod  _  0 = prod
        iter !prod !a !b
          | (b .&. 1) /= 0  = iter (prod `add_f2` a) a' b'
          | otherwise       = iter  prod             a' b'
          where a' = a `shiftL` 1
                b' = b `shiftR` 1

add_f2 :: (Bits a, Num a) => a -> a -> a
add_f2 x y = x `xor` y

divMod_f2 :: (GF2Polynomial a, Bits a, Integral a) => a -> a -> (a, a)
divMod_f2 _ 0 = error "divide by 0"
divMod_f2 x y | x < 0     = divMod_f2 (-x) y
              | y < 0     = divMod_f2 x (-y)
              | otherwise = iter n (n-m) 0 x
  where n = fromIntegral $ highestBit x
        m = fromIntegral $ highestBit y
        iter !t !s !q !r | s < 0     = (q,r)
                         | b /= 0    = iter (t-1) (s-1) q' r'
                         | otherwise = iter (t-1) (s-1) q r
          where b  = r .&.   (1 `shiftL` t)
                q' = q .|.   (1 `shiftL` s)
                r' = r `xor` (y `shiftL` s)

-- |Calculate <http://mathworld.wolfram.com/MultiplicativeOrder.html multiplicative order>
-- of \(x\) (mod \(P(x)\)). 0 will be returned for an even number.
--
-- For given polynomial \(P(x)\) \((P(0)\neq{0})\), multiplicative
-- order \(e\) is the smallest number that satisfies \(x^e \equiv 1\ (\mathrm{mod}\ P(x))\).
--
-- Following condition is satisfied:
--
-- prop> (2 ^ (order k)) `mod` k == 1
order :: Integer -> Integer
order x | x .&. 1 == 0  = 0
        | otherwise     = iter 1 2
  where iter !n !p | r == 1    = n
                   | otherwise = iter (n+1) (r*2)
          where r = p `mod` x
