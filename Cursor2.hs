{-# LANGUAGE TypeFamilies,TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module Cursor2 where

import Data.Map (fromListWith, assocs, elems, fromList,union)
import Data.List hiding (union)
import Control.Arrow ((&&&))
import System.Random
import Control.DeepSeq
import Control.Monad
import Corr
import Schedule

type Revol = [Double] 

data Proj = Sh Double | Re Double deriving (Show,Read)

apply :: Proj -> Double -> Double
apply (Sh x) y = let 
	z = x + y 
	n = floor z
	in z - fromIntegral n
apply (Re x) y = let 
	z = x * y 
	n = floor z
	in z - fromIntegral n

trace ::  Revol -> [Proj] -> [Revol]
trace  = scanl (\xs y -> map (apply y) xs)

data Window a = Window Int Int Integer (Window a) a

type instance Variator Window [Double] = ()
type instance Goodness Window [Double] = Integer

instance Cursor Window [Double]  where
        variate () (Window _ _ _ nxt _) = nxt
        point (Window _ _ v _ r) = (r, v)

window :: Int -> [Revol] -> [Revol]
window n = map (concat . take n) . tails 

dist :: Int -> Revol -> [Int]
dist n = elems . flip union (fromList $ zip [0..n-1] $ repeat 0) . fromListWith (+) . map (floor . (/k) &&& const 1) where
        k = 1/fromIntegral n

normalize :: [Int] -> [Double] 
normalize xs = let 
        ys = map fromIntegral xs
        mx = maximum ys
        mmx = minimum ys
        in map (\y -> (y - mmx)/(mx - mmx)) ys
mkRevol :: Int -> Int -> Int -> Int -> [Proj] -> Window [Double]

mkRevol se op bu ps f = let
        newW (r:rs) = let 
                d = dist bu $ r 
                in Window op bu (val . L bu . map fromIntegral $ d) (newW rs) (normalize d)
        ls = randomRs (0, length f - 1) (mkStdGen se)
        in newW $ window op $ trace (take ps [0,1/fromIntegral ps.. ]) $ map (f!!) ls
