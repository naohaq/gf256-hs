{-# OPTIONS_GHC -Wall #-}

module Data.ExtensionF2 where

class ExtensionF2 k where
  generator :: k -> Int
  fromInt :: Int -> k
  toInt :: k -> Int
  log2 :: k -> Int
  pow2 :: Int -> k
  degree :: k -> Int
  primitives :: k -> [Int]

