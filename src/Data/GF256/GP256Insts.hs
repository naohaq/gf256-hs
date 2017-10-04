{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- module      :  Data.GF256.GP256Insts
-- Copyright   :  (c) 2017 Naoyuki MORITA
-- License     :  MIT
--
-- Maintainer  :  naoyuki.morita@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (TemplateHaskell)
--
-----------------------------------------------------------------------------

module Data.GF256.GP256Insts
  ( PP285, PP299, PP301, PP333
  , PP351, PP355, PP357, PP361
  , PP369, PP391, PP397, PP425
  , PP451, PP463, PP487, PP501
  ) where

import Data.GF256.GenPoly256

-- | A primitive polynomial of \(\mathrm{GF}(2^8)\): \(x^8+x^6+x^5+x^4+1\)
declInstGP256 285

-- | A primitive polynomial of \(\mathrm{GF}(2^8)\): \(x^8+x^7+x^5+x^3+1\)
declInstGP256 299

-- | A primitive polynomial of \(\mathrm{GF}(2^8)\): \(x^8+x^6+x^5+x^3+1\)
declInstGP256 301

-- | A primitive polynomial of \(\mathrm{GF}(2^8)\): \(x^8+x^6+x^3+x^2+1\)
declInstGP256 333

-- | A primitive polynomial of \(\mathrm{GF}(2^8)\): \(x^8+x^6+x^4+x^3+x^2+x+1\)
declInstGP256 351

-- | A primitive polynomial of \(\mathrm{GF}(2^8)\): \(x^8+x^6+x^5+x+1\)
declInstGP256 355

-- | A primitive polynomial of \(\mathrm{GF}(2^8)\): \(x^8+x^6+x^5+x^2+1\)
declInstGP256 357

-- | A primitive polynomial of \(\mathrm{GF}(2^8)\): \(x^8+x^6+x^5+x^3+1\)
declInstGP256 361

-- | A primitive polynomial of \(\mathrm{GF}(2^8)\): \(x^8+x^6+x^5+x^4+1\)
declInstGP256 369

-- | A primitive polynomial of \(\mathrm{GF}(2^8)\): \(x^8+x^7+x^2+x+1\)
declInstGP256 391

-- | A primitive polynomial of \(\mathrm{GF}(2^8)\): \(x^8+x^7+x^3+x^2+1\)
declInstGP256 397

-- | A primitive polynomial of \(\mathrm{GF}(2^8)\): \(x^8+x^7+x^5+x^3+1\)
declInstGP256 425

-- | A primitive polynomial of \(\mathrm{GF}(2^8)\): \(x^8+x^7+x^6+x+1\)
declInstGP256 451

-- | A primitive polynomial of \(\mathrm{GF}(2^8)\): \(x^8+x^7+x^6+x^3+x^2+x+1\)
declInstGP256 463

-- | A primitive polynomial of \(\mathrm{GF}(2^8)\): \(x^8+x^7+x^6+x^5+x^2+x+1\)
declInstGP256 487

-- | A primitive polynomial of \(\mathrm{GF}(2^8)\): \(x^8+x^7+x^6+x^5+x^4+x^2+1\)
declInstGP256 501

-- EOF
