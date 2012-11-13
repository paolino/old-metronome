{-# LANGUAGE TypeFamilies,TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module Cursor3 where

import Data.Map (fromListWith, assocs, elems, fromList,union)
import Data.List hiding (union)
import Control.Arrow ((&&&))
import System.Random
import Control.DeepSeq
import Control.Monad
import Control.Monad.Random hiding (fromList)

type Dist = [Int]
type Buckets = Int 

data Window  = Window {
        buckets :: Buckets,
        value :: Dist,
        next :: Window
        } 

dist :: Int -> [Double] -> Dist
dist n = elems . flip union (fromList $ zip [0..n-1] $ repeat 0) . fromListWith (+) . map (floor . (/k) &&& const 1) where
        k = 1/fromIntegral n

mkDist :: Int -> Int -> StdGen -> Dist
mkDist ps bu g = dist bu . flip evalRand g $ replicateM ps $ getRandomR (0,1)


normalize :: Dist -> [Double] 
normalize xs = let 
        ys = map fromIntegral xs
        mx = maximum ys
        mmx = minimum ys
        in map (\y -> (y - mmx)/(mx - mmx)) ys

mkWindow :: Int -> Int -> Int -> Int -> Window 
mkWindow s op bu ps = let
        newW (l:ls) q (r:rs) = let 
                q' = zipWith3 (\lx qx rx -> qx - lx + rx) l q r
                in Window bu q' (newW ls q' rs)
        gs = unfoldr (Just . split) (mkStdGen s)
        ls = map (mkDist ps bu) gs
        rs = drop op ls
        q = foldr (zipWith (+)) (repeat 0) $ take op ls
        in newW ls q rs
