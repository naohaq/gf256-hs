{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- module      :  Data.GF2Extension
-- Copyright   :  (c) 2017 Naoyuki MORITA
-- License     :  MIT
--
-- Maintainer  :  naoyuki.morita@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Data.GF2Extension
  ( GF2Extension (..)
  ) where

-- | Type class for extension field of GF(2)
class GF2Extension k where
  -- | Returns the generator polynomial as Int value.
  generator :: k -> Int
  -- | Generate an element of the field from an Int value.
  fromInt :: Int -> k
  -- | Reinterprets an element of the field as integer, and returns it as Int.
  toInt :: k -> Int
  -- | Calculates non-negative integer \(n\), that satisfies \(2^n=x\) for given \(x\).
  log2 :: k -> Int
  -- | Calculates \(2^n\) for given \(n\).
  pow2 :: Int -> k
  -- | Returns the extension field degree.
  degree :: k -> Int

-- EOF
