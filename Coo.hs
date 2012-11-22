{-# LANGUAGE FlexibleContexts #-}
module Coo where

import Data.List
import Data.Ord
-- import Control.Parallel.Strategies

-- import Control.DeepSeq

import Cursor3
import Corr (val,selfval) 
import Control.Arrow

choose :: [Window] -> Window -> Window -> Window
choose ys x y = let     vy  = val ys y
                        vx = val ys x
                        in if  vx > vy then x else y

update :: Int -> [Window] -> [Window] -> ([Window],[Window])
update l xs ys = let
        xs' = map next xs
        zs = map (take l) . tail $ tails $ cycle ys
        ys'' =  zipWith3 choose zs xs' ys
        in (xs', ys'')

search  :: [Window] -> [[Window]]
search xs = map (sortW . snd) $ iterate (uncurry $ update (length xs)) (xs, xs)
        
sortW :: [Window] -> [Window]
sortW = sortBy (flip $ comparing selfval)
                



