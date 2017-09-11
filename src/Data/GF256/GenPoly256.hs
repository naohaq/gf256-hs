{-# OPTIONS_GHC -Wall #-}

module Data.GF256.GenPoly256
  (GenPoly256 (..),
   PP285, PP299, PP301, PP333, PP351, PP355, PP357, PP361,
   PP369, PP391, PP397, PP425, PP451, PP463, PP487, PP501
  ) where

import TypeLevel.Number.Nat

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

class GenPoly256 k where
  genVal :: k
  genInt :: k -> Int

instance GenPoly256 PP285 where
  genVal = PP285 undefined
  genInt (PP285 x) = toInt x

instance GenPoly256 PP299 where
  genVal = PP299 undefined
  genInt (PP299 x) = toInt x

instance GenPoly256 PP301 where
  genVal = PP301 undefined
  genInt (PP301 x) = toInt x

instance GenPoly256 PP333 where
  genVal = PP333 undefined
  genInt (PP333 x) = toInt x

instance GenPoly256 PP351 where
  genVal = PP351 undefined
  genInt (PP351 x) = toInt x

instance GenPoly256 PP355 where
  genVal = PP355 undefined
  genInt (PP355 x) = toInt x

instance GenPoly256 PP357 where
  genVal = PP357 undefined
  genInt (PP357 x) = toInt x

instance GenPoly256 PP361 where
  genVal = PP361 undefined
  genInt (PP361 x) = toInt x

instance GenPoly256 PP369 where
  genVal = PP369 undefined
  genInt (PP369 x) = toInt x

instance GenPoly256 PP391 where
  genVal = PP391 undefined
  genInt (PP391 x) = toInt x

instance GenPoly256 PP397 where
  genVal = PP397 undefined
  genInt (PP397 x) = toInt x

instance GenPoly256 PP425 where
  genVal = PP425 undefined
  genInt (PP425 x) = toInt x

instance GenPoly256 PP451 where
  genVal = PP451 undefined
  genInt (PP451 x) = toInt x

instance GenPoly256 PP463 where
  genVal = PP463 undefined
  genInt (PP463 x) = toInt x

instance GenPoly256 PP487 where
  genVal = PP487 undefined
  genInt (PP487 x) = toInt x

instance GenPoly256 PP501 where
  genVal = PP501 undefined
  genInt (PP501 x) = toInt x
