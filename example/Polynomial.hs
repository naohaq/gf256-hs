{- -*- coding: utf-8-unix -*- -}

module Polynomial
  ( mul
  , add
  , sub
  , divMod
  , mod
  , normalize
  , apply
  , (⊗), (⊕), (⊖)
  ) where

import Prelude hiding (divMod, mod)

normalize :: (Eq a, Num a) => [a] -> [a]
normalize [] = []
normalize (x:xs) | x == 0    = normalize xs
                 | otherwise = x:xs

mul :: (Num a) => [a] -> [a] -> [a]
mul  []    _  = []
mul (x:xs) ys = add' (map (x*) ys) (0 : mul xs ys)
  where add' [] ws = ws
        add' zs [] = zs
        add' (z:zs) (w:ws) = z + w : add' zs ws

infixl 7 ⊗
(⊗) :: (Num a) => [a] -> [a] -> [a]
xs ⊗ ys = mul xs ys

add :: (Num a) => [a] -> [a] -> [a]
add  []  ys = ys
add  xs  [] = xs
add  xs  ys = zipWith (+) xs' ys'
  where nx = length xs
        ny = length ys
        m  = max nx ny
        xs' = foldr (:) xs $ replicate (m - nx) 0
        ys' = foldr (:) ys $ replicate (m - ny) 0

infixl 6 ⊕
(⊕) :: (Num a) => [a] -> [a] -> [a]
xs ⊕ ys = add xs ys

sub :: (Num a) => [a] -> [a] -> [a]
sub xs ys = add xs (map negate ys)

infixl 6 ⊖
(⊖) :: (Num a) => [a] -> [a] -> [a]
xs ⊖ ys = sub xs ys

divMod :: (Num a, Fractional a) => [a] -> [a] -> ([a],[a])
divMod _  [] = error "Divide by zero."
divMod [] _  = ([],[])
divMod z@(x:xs) w@(y:ys)
  | nz < nw    = ([], z)
  | otherwise  = (q:qs, r')
  where nz = length z
        nw = length w
        q = x / y
        r = (take (nw-1) xs ⊖ map (q*) ys) ++ drop (nw-1) xs
        (qs,r') = divMod r w

mod :: (Num a, Fractional a) => [a] -> [a] -> [a]
mod z w = snd $ divMod z w

apply :: (Num a) => [a] -> a -> a
apply xs y = sum $ zipWith (*) (reverse xs) $ iterate (y*) 1

-- EOF
