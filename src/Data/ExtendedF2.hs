{-# OPTIONS_GHC -Wall #-}

module Data.ExtendedF2 where

class ExtendedF2 k where
  generator :: k -> Int
  fromInt :: Int -> k
  toInt :: k -> Int
  log2 :: k -> Int
  pow2 :: Int -> k
  primitives :: k -> [Int]

