{-# OPTIONS_GHC -Wall #-}

module ReedSolomon
  ( code_N
  , code_2t
  , gen_poly
  , calcChecksum
  , calcSyndrome
  , errLocator
  , solveErrLocations
  , errMatrix
  , solveErrMatrix
  , correctErrors
  ) where

import qualified Polynomial as P

code_N :: Int
code_N = 40

code_2t :: Int
code_2t = 18

gen_poly :: (Num k) => (Int -> k) -> Int -> [k]
gen_poly fp x = foldr P.mul [1] $ map e [1..x]
  where e j = [1, negate (fp j)]

calcChecksum :: (Integral a, Num k, Fractional k) => [k] -> [a] -> [k]
calcChecksum gp ms = mp `P.mod` gp
  where mp = map (fromInteger . fromIntegral) ms

calcSyndrome :: (Num k) => (Int -> k) -> [k] -> [k]
calcSyndrome fp xs = [P.apply xs (fp j) | j <- [1..code_2t]]

solveErrLocations :: (Num k, Eq k) => (Int -> k) -> [k] -> [Int]
solveErrLocations fp csr = [j | j <- [0..(code_N-1)], P.apply csr (fp j) == 0]

errLocator :: (Num k, Fractional k, Eq k) => [k] -> [k]
errLocator ss = reverse cs
  where (_, cs, _, _) = foldr (errLocator_sub ss) (1,[1],[1],1) [(code_2t-1),(code_2t-2)..0]

errLocator_sub :: (Num k, Fractional k, Eq k) => [k] -> Int -> (Int,[k],[k],k) -> (Int,[k],[k],k)
errLocator_sub ss n (m,cs,bs,b) | d == 0    = (m+1,cs ,bs,b)
                                | otherwise = (1  ,cs',cs,d)
  where k = length cs - 1
        d = sum $ zipWith (*) cs (drop (n-k) ss)
        e = d/b
        bs' = map (e*) bs `P.mul` (1 : replicate m 0)
        cs' = cs `P.sub` bs'

errMatrix :: (Num k) => (Int -> k) -> [Int] -> [k] -> [[k]]
errMatrix fp locs ss = [map (fp . (j*)) locs ++ [ss !! (j-1)] | j <- [1..t]]
  where t = length locs

solveErrMatrix :: (Num k, Fractional k, Eq k) => [[k]] -> [k]
solveErrMatrix mtx = map (head . drop n) $ backwardSubst n $ forwardErase n mtx
  where n = length mtx

replaceAt :: Int -> a -> [a] -> [a]
replaceAt _ _ [] = []
replaceAt 0 y (_:xs) = y:xs
replaceAt n y (x:xs) = x : replaceAt (n-1) y xs

addRowTo :: (Num a) => Int -> Int -> a -> [[a]] -> [[a]]
addRowTo x y w rows = replaceAt y r1' rows
  where r0 = rows !! x
        r1 = rows !! y
        r1' = zipWith ((+) . (w*)) r0 r1

multRow :: (Num a) => Int -> a -> [[a]] -> [[a]]
multRow x w rows = replaceAt x r' rows
  where r = rows !! x
        r' = map (w*) r

eraseCols :: (Num a, Fractional a, Eq a) => Int -> Int -> [[a]] -> [[a]]
eraseCols n j mtx = foldr ers mtx' [(j+1)..(n-1)]
  where w = (mtx !! j) !! j
        mtx' = multRow j (1/w) mtx
        ers k m | v /= 0    = addRowTo j k (negate v) m
                | otherwise = m
          where v = (m !! k) !! j

forwardErase :: (Num a, Fractional a, Eq a) => Int -> [[a]] -> [[a]]
forwardErase n mtx = foldr (eraseCols n) mtx (reverse [0..(n-1)])

substCol :: (Num a, Fractional a, Eq a) => Int -> [[a]] -> [[a]]
substCol j mtx = foldr ers mtx [0..(j-1)]
  where ers k m | v /= 0    = addRowTo j k (negate v) m
                | otherwise = m
          where v = (m !! k) !! j

backwardSubst :: (Num a, Fractional a, Eq a) => Int -> [[a]] -> [[a]]
backwardSubst n mtx = foldr substCol mtx [1..(n-1)]

correctErrors :: (Num a) => [(Int,a)] -> [a] -> [a]
correctErrors ers xs = iter 0 ers xs
  where iter _ _  [] = []
        iter _ [] ys = ys
        iter j ((k,e):es) (y:ys) | j == k    = (y - e) : iter (j+1) es ys
                                 | otherwise = y : iter (j+1) ((k,e):es) ys

-- EOF
