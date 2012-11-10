{-# LANGUAGE TypeFamilies,TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module Cursor3 where

import Data.Map (fromListWith, assocs, elems, fromList,union)
import Data.List hiding (union)
import Control.Arrow ((&&&))
import System.Random
import Control.DeepSeq
import Control.Monad
import Control.Monad.Random hiding (fromList)
import Corr
import Schedule

type Revol = [Double] 

dist :: Int -> Revol -> [Int]
dist n = elems . flip union (fromList $ zip [0..n-1] $ repeat 0) . fromListWith (+) . map (floor . (/k) &&& const 1) where
        k = 1/fromIntegral n

newDist :: Int -> Int -> StdGen -> [Int]
newDist ps bu g = dist bu . flip evalRand g $ replicateM ps $ getRandomR (0,1)

data Window a = Window Int Int Integer (Window a) a

type instance Variator Window [Double] = ()
type instance Goodness Window [Double] = Integer

instance Cursor Window [Double]  where
        variate () (Window _ _ _ nxt _) = nxt
        point (Window _ _ v _ r) = (r, v)


normalize :: [Int] -> [Double] 
normalize xs = let 
        ys = map fromIntegral xs
        mx = maximum ys
        mmx = minimum ys
        in map (\y -> (y - mmx)/(mx - mmx)) ys

mkRevol :: Int -> Int -> Int -> Int -> StdGen -> Window [Double]
mkRevol se op bu ps f = let
        newW (l:ls) q (r:rs) = let 
                q' = zipWith3 (\lx qx rx -> qx - lx + rx) l q r
                in Window op bu (val . L bu . map fromIntegral $ q') (newW ls q' rs) (normalize q')
        gs = unfoldr (Just . split) (mkStdGen se)
        ls = map (newDist ps bu) gs
        rs = drop op ls
        q = foldr (zipWith (+)) (repeat 0) $ take op ls
        in newW ls q rs
