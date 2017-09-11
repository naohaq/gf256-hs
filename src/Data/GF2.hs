{-# OPTIONS_GHC -Wall #-}

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

mul :: (Bits a, Num a) => a -> a -> a
mul x y = iter 0 x y
  where iter prod _ 0 = prod
        iter prod a b
          | (b .&. 1) /= 0  = iter (prod `xor` a) a' b'
          | otherwise       = iter  prod          a' b'
          where a' = a `shiftL` 1
                b' = b `shiftR` 1

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

mod :: (Integral a, Bits a) => a -> a -> a
mod x y = snd $ divMod x y

order :: Integer -> Integer
order x = iter 1 2
  where iter n p | r == 0    = n
                 | otherwise = iter (n+1) (p*2)
          where r = mod (p+1) x
