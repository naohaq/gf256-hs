
module Polynomial
  ( mul
  , add
  , mod
  ) where

import Prelude hiding (mod)

mul :: (Num a) => [a] -> [a] -> [a]
mul  []    _  = []
mul (x:xs) ys = add (map (x*) ys) (0 : mul xs ys)

add :: (Num a) => [a] -> [a] -> [a]
add  []     ys    = ys
add  xs     []    = xs
add (x:xs) (y:ys) = x + y : add xs ys

mod :: (Num a, Fractional a) => [a] -> [a] -> [a]
mod  _   []  = error "Divide by zero."
mod  []  _   = []
mod z@(x:xs) w@(y:ys)
  | length z < length w  = z
  | otherwise            = mod r w
  where q = negate $ x / y
        r = xs `add` map (q*) ys

-- EOF
