{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

module Data.GF256.GP256Insts
  (PP285, PP299, PP301, PP333, PP351, PP355, PP357, PP361
  ,PP369, PP391, PP397, PP425, PP451, PP463, PP487, PP501
  ) where

import TypeLevel.Number.Nat

import Data.GF256.GenPoly256

newtype PP285 = PP285 (I (O (I (I (I (O (O (O (I Z)))))))))
newtype PP299 = PP299 (I (I (O (I (O (I (O (O (I Z)))))))))
newtype PP301 = PP301 (I (O (I (I (O (I (O (O (I Z)))))))))
newtype PP333 = PP333 (I (O (I (I (O (O (I (O (I Z)))))))))
newtype PP351 = PP351 (I (I (I (I (I (O (I (O (I Z)))))))))
newtype PP355 = PP355 (I (I (O (O (O (I (I (O (I Z)))))))))
newtype PP357 = PP357 (I (O (I (O (O (I (I (O (I Z)))))))))
newtype PP361 = PP361 (I (O (O (I (O (I (I (O (I Z)))))))))
newtype PP369 = PP369 (I (O (O (O (I (I (I (O (I Z)))))))))
newtype PP391 = PP391 (I (I (I (O (O (O (O (I (I Z)))))))))
newtype PP397 = PP397 (I (O (I (I (O (O (O (I (I Z)))))))))
newtype PP425 = PP425 (I (O (O (I (O (I (O (I (I Z)))))))))
newtype PP451 = PP451 (I (I (O (O (O (O (I (I (I Z)))))))))
newtype PP463 = PP463 (I (I (I (I (O (O (I (I (I Z)))))))))
newtype PP487 = PP487 (I (I (I (O (O (I (I (I (I Z)))))))))
newtype PP501 = PP501 (I (O (I (O (I (I (I (I (I Z)))))))))

declInstGP256 ''PP285 'PP285
declInstGP256 ''PP299 'PP299
declInstGP256 ''PP301 'PP301
declInstGP256 ''PP333 'PP333
declInstGP256 ''PP351 'PP351
declInstGP256 ''PP355 'PP355
declInstGP256 ''PP357 'PP357
declInstGP256 ''PP361 'PP361
declInstGP256 ''PP369 'PP369
declInstGP256 ''PP391 'PP391
declInstGP256 ''PP397 'PP397
declInstGP256 ''PP425 'PP425
declInstGP256 ''PP451 'PP451
declInstGP256 ''PP463 'PP463
declInstGP256 ''PP487 'PP487
declInstGP256 ''PP501 'PP501

-- EOF
