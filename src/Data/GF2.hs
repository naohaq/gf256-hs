{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- module      :  Data.GF2
-- Copyright   :  (c) 2017 Naoyuki MORITA
-- License     :  MIT
--
-- Maintainer  :  naoyuki.morita@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Arithmetic operations treating integral values as polynomials over GF(2).
--
-- For example, the number @1234@ (@10011010010@ in binary) can be seen as:
--
-- > x^10 + x^7 + x^6 + x^4 + x
--
-- In this view, product of two integers is calculated as polynomial
-- multiplication. e.g. @7@ (= @x^2+x+1@) @\`mul\`@ @6@ (= @x^2+x@) would be:
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

module Data.GF2
  ( mul
  , divMod
  , mod
  , order
  ) where

import Prelude hiding (mod, divMod)
import Data.Bits

highBit :: (Bits a, Num a, Integral b) => a -> b
highBit 0 = 0
highBit 1 = 0
highBit n = 1 + highBit (n `shiftR` 1)

-- |Multiplies two numbers as polynomials over GF(2).
mul :: (Bits a, Num a) => a -> a -> a
mul x y = iter 0 x y
  where iter prod _ 0 = prod
        iter prod a b
          | (b .&. 1) /= 0  = iter (prod `xor` a) a' b'
          | otherwise       = iter  prod          a' b'
          where a' = a `shiftL` 1
                b' = b `shiftR` 1

-- |Calculates the quotient and the remainder of A/B.
--
-- For given \(A(x)\), \(B(x)\neq{0}\), there exists
-- the quotient \(Q(x)\) and the remainder \(R(x)\)
-- such that: \(A(x) = Q(x)\cdot{B}(x) + R(x)\).
divMod :: (Integral a, Bits a) => a -> a -> (a, a)
divMod _ 0 = undefined
divMod x y | x < 0     = divMod (-x) y
           | y < 0     = divMod x (-y)
           | otherwise = iter n (n-m) 0 x
  where n = highBit x
        m = highBit y
        iter t s q r | s < 0     = (q,r)
                     | b /= 0    = iter (t-1) (s-1) q' r'
                     | otherwise = iter (t-1) (s-1) q r
          where b  = r .&.   (1 `shiftL` t)
                q' = q .|.   (1 `shiftL` s)
                r' = r `xor` (y `shiftL` s)

-- |The function returns only remainder part of division.
mod :: (Integral a, Bits a) => a -> a -> a
mod x y = snd $ divMod x y

-- |Returns <http://mathworld.wolfram.com/PolynomialOrder.html polynomial order>
-- of argument. 0 will be returned for an even number.
--
-- Following condition is satisfied:
--
-- prop> (2 ^ (order k)) `mod` k == 1
order :: Integer -> Integer
order x | x .&. 1 == 0  = 0
        | otherwise     = iter 1 2
  where iter n p | r == 0    = n
                 | otherwise = iter (n+1) (p*2)
          where r = (p+1) `mod` x
