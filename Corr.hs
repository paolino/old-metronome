module Corr where

import Data.List 
import Data.Ord (comparing)
import Control.Arrow (first)


data L = L Int [Integer]
val0 :: L -> [(Integer,Integer)]
val0 (L n x) = filter ((`elem` reg) . fst) . zip [1..] $ map sum $ zipWith (zipWith (*))  (replicate ((n `div` 2)) x) (tail . tails $ cycle x)

val (L _ []) = 0
val x = sum .  map (\(x,y) -> x * y) . val0 $ x

reg = [2,4,8,16,3,6,12,24,32]
report = sortBy (flip $ comparing snd) . val0




