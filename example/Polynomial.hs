
module Polynomial
  ( mul
  , add
  , divMod
  , mod
  , normalize
  ) where

import Prelude hiding (divMod, mod)

normalize :: (Eq a, Num a) => [a] -> [a]
normalize [] = []
normalize (x:xs) | x == 0    = normalize xs
                 | otherwise = x:xs

mul :: (Num a) => [a] -> [a] -> [a]
mul  []    _  = []
mul (x:xs) ys = add (map (x*) ys) (0 : mul xs ys)

add :: (Num a) => [a] -> [a] -> [a]
add  []     ys    = ys
add  xs     []    = xs
add (x:xs) (y:ys) = x + y : add xs ys

divMod :: (Num a, Fractional a) => [a] -> [a] -> ([a],[a])
divMod _  [] = error "Divide by zero."
divMod [] _  = ([],[])
divMod z@(x:xs) w@(y:ys)
  | length z < length w  = ([], z)
  | otherwise            = (q:qs, r')
  where q = negate $ x / y
        r = xs `add` map (q*) ys
        (qs,r') = divMod r w

mod :: (Num a, Fractional a) => [a] -> [a] -> [a]
mod  _   []  = error "Divide by zero."
mod  []  _   = []
mod z@(x:xs) w@(y:ys)
  | length z < length w  = z
  | otherwise            = mod r w
  where q = negate $ x / y
        r = xs `add` map (q*) ys

-- EOF
