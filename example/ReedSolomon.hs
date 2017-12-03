
module Main where

import Data.GF2Extension
import Data.GF256
import Data.Bits
import qualified Data.Word as W
import qualified Polynomial as P

import MyUtil

type F256 = GF256 PP301

code_N :: Int
code_N = 40

code_2t :: Int
code_2t = 18

gen_ecc200 :: Int -> [F256]
gen_ecc200 x = foldr P.mul [1] $ map e [1..x]
  where e :: Int -> [F256]
        e k = [1, pow2 k]

gp_pp301 :: [F256]
gp_pp301 = gen_ecc200 code_2t

cws :: [W.Word8]
cws = [0x8e, 0x32, 0x2e, 0xbe, 0x92, 0x35, 0x2e, 0x83,
       0x21, 0x73, 0x66, 0x77, 0x21, 0x86, 0x21, 0x92,
       0x8a, 0x4c, 0x4a, 0x82, 0x86, 0x37, 0x24, 0xa3,
       0xa7, 0x8e, 0xf0, 0x7b, 0xea, 0x38, 0xe7, 0x57,
       0xcd, 0x1d, 0x7a, 0x7e, 0xd8, 0xb2, 0x24, 0xe9]

ews :: [W.Word8]
ews = [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08,
       0xc0, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00,
       0x00, 0x00, 0x00, 0x21, 0x00, 0x00, 0x00, 0x00,
       0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
       0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]

check_ecc200 :: (Integral a) => [a] -> [F256]
check_ecc200 ds = dp `P.mod` gp_pp301
  where dp = map (fromInt.fromIntegral) ds

calcSyndrome :: [F256] -> [F256]
calcSyndrome xs = [P.apply xs (pow2 k) | k <- [1..code_2t]]

errMatrix :: [Int] -> [F256] -> [[F256]]
errMatrix locs ss = [map (pow2 . (k*)) locs ++ [ss !! (k-1)] | k <- [1..t]]
  where t = length locs

solveErrLocations :: [F256] -> [Int]
solveErrLocations csr = [k | k <- [0..(code_N-1)], P.apply csr (pow2 k) == 0]

errLocator :: [F256] -> [F256]
errLocator ss = reverse cs
  where (_, cs, _, _) = foldr (errLocator_sub ss) (1,[1],[1],1) [(code_2t-1),(code_2t-2)..0]

errLocator_sub :: [F256] -> Int -> (Int,[F256],[F256],F256) -> (Int,[F256],[F256],F256)
errLocator_sub ss n (m,cs,bs,b) | d == 0    = (m+1,cs ,bs,b)
                                | otherwise = (1  ,cs',cs,d)
  where k = length cs - 1
        d = sum $ zipWith (*) cs (drop (n-k) ss)
        e = d/b
        bs' = map (e*) bs `P.mul` (1 : replicate m 0)
        cs' = cs `P.add` bs'

solveErrMatrix :: Int -> [[F256]] -> [F256]
solveErrMatrix n mtx = map (head . drop n) $ backwardSubst n $ forwardErase n mtx

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
        ers k m | v /= 0    = addRowTo j k v m
                | otherwise = m
          where v = (m !! k) !! j

forwardErase :: (Num a, Fractional a, Eq a) => Int -> [[a]] -> [[a]]
forwardErase n mtx = foldr (eraseCols n) mtx (reverse [0..(n-1)])

substCol :: (Num a, Fractional a, Eq a) => Int -> Int -> [[a]] -> [[a]]
substCol n j mtx = foldr ers mtx [0..(j-1)]
  where ers k m | v /= 0    = addRowTo j k v m
                | otherwise = m
          where v = (m !! k) !! j

backwardSubst :: (Num a, Fractional a, Eq a) => Int -> [[a]] -> [[a]]
backwardSubst n mtx = foldr (substCol n) mtx [1..(n-1)]

correctErrors :: (Integral a, Bits a) => [(Int,a)] -> [a] -> [a]
correctErrors ers xs = iter 0 ers xs
  where iter _ _  [] = []
        iter _ [] ys = ys
        iter j ((k,e):es) (y:ys) | j == k    = (y `xor` e) : iter (j+1) es ys
                                 | otherwise = y : iter (j+1) ((k,e):es) ys

toWord8 :: F256 -> W.Word8
toWord8 x = (fromIntegral . toInt) x

showHex :: [F256] -> String
showHex = joinStr " " . map (fromW8toHex . toWord8)

dumpMsg :: [W.Word8] -> [String]
dumpMsg [] = []
dumpMsg xs = (joinStr " " . map fromW8toHex) hs : dumpMsg ts
  where hs = take 8 xs
        ts = drop 8 xs

main :: IO ()
main = do
  let rws = zipWith xor cws ews
  putStrLn $ "RS Code: (" ++ show code_N ++ "," ++ show (code_N - code_2t) ++ ")"
  putStrLn $ "Received message: "
  mapM_ (putStrLn . ("   " ++)) (dumpMsg rws)
  let csum = check_ecc200 rws
  let synd = calcSyndrome csum
  putStrLn $ "Syndromes: " ++ showHex synd
  let sigma_r = errLocator synd
  putStrLn $ "Error locator: " ++ showHex sigma_r
  let locs = solveErrLocations sigma_r
  let locs_r = [code_N-1-k | k <- reverse locs]
  putStrLn $ "Error locations: " ++ show locs_r
  let mtx = errMatrix locs synd
  putStrLn "Error matrix:"
  mapM_ (putStrLn . ("   [ " ++) . (++ " ]") . showHex) mtx
  let evs = solveErrMatrix (length locs) mtx
  let evs_r = reverse evs
  putStrLn $ "Error values: " ++ showHex evs_r
  let rws_corr = correctErrors (zip locs_r (map toWord8 evs_r)) rws
  putStrLn $ "Corrected message: "
  mapM_ (putStrLn . ("   " ++)) (dumpMsg rws_corr)

-- EOF
