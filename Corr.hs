module Corr where

import Data.List 
import Data.Ord (comparing)
import Control.Arrow (first)
import Cursor3

data L a = L Int [a]
no (L i _) = i 
val0 :: Num a => L a -> L a -> [a]
val0 (L n x) (L m y) =  map sum $ zipWith (zipWith (*))  (replicate n x) (tails $ cycle y)


k = 1


summa x y = sum .  map snd . filter ((`elem` reg (no x) (no y)) . fst) . zip [0..] .val0 x $ y

val zs w = (k * fromIntegral (length zs) * summa x x) + sum (map (summa x . toL) zs) where x = toL w
selfval w =  summa x x where x = toL w
toL :: Window -> L Integer
toL (Window bu q _) =  L bu . map fromIntegral $ q

reg n m = takeWhile (< min n m) [8,16..]

        -- take (min n m `div` 2) [0..]
-- report = sortBy (flip $ comparing snd) . val0

primes :: [Integer]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x|x <- xs, x `mod` 2 == 0 || (x `mod` 3 == 0 && x `mod` 9 > 0)]

badcorr :: [Double] -> [Double] -> Int
badcorr x y = fst . minimumBy (comparing snd) $ zip [0..] (val0 (L (length x) x) (L (length y) y))



