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

update :: [(Integer,Window)] -> [(Integer,Window)]
update xs = let
        ((v,x'):xs') = sortBy (comparing fst) xs
        qs = iterate (\(_,q) -> (val (map snd xs') q,next q)) (v,x')
        (q':_) = dropWhile ((<= v) . fst) qs
        in q':xs'

search  :: [Window] -> [[Window]]
search xs =  map (map snd) $ iterate update (zip (repeat 0) xs)
        
sortW :: [Window] -> [Window]
sortW = sortBy (flip $ comparing selfval)
                



