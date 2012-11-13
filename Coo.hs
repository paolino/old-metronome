{-# LANGUAGE FlexibleContexts #-}
module Coo where

import Data.List
-- import Control.Parallel.Strategies

-- import Control.DeepSeq

import Cursor3
import Corr (val) 

choose :: [Window] -> Window -> Window -> Window
choose ys x y = if  val ys x > val ys y then x else y

update :: Int -> [Window] -> [Window] -> ([Window],[Window])
update l xs ys = let
        xs' = map next xs
        zs = map (take l) . tail $ tails $ cycle ys
        ys' =  zipWith3 choose zs xs' ys
        in (xs', ys')

search  :: [Window] -> [[Window]]
search xs = map snd $ iterate (uncurry $ update (length xs)) (xs,xs)
        
                



