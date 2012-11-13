module Corr where

import Data.List 
import Data.Ord (comparing)
import Control.Arrow (first)
import Cursor3

data L = L Int [Integer]

val0 :: L -> L -> [(Integer,Integer)]
val0 (L n x) (L m y) = filter ((`elem` reg) . fst) . zip [0..] $ map sum $ zipWith (zipWith (*))  (replicate n x) (tails $ cycle y)


k = 1


summa x = sum .  map (\(x,y) -> (1 + x) * y) . val0 x

val zs w = (k * fromIntegral (length zs) * summa x x) + sum (map (summa x . toL) zs) where x = toL w

toL :: Window -> L
toL (Window bu q _) =  L bu . map fromIntegral $ q

reg = [0,2,3,4,6,8,12,16,24,32]
-- report = sortBy (flip $ comparing snd) . val0





