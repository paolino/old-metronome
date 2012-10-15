{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances #-}


module Track where

import Control.Monad (guard)
import Data.Ratio (Rational, (%))
import Data.Maybe (fromMaybe, catMaybes)
import Data.List (mapAccumL)
import Control.Arrow (first)
import Data.Monoid (Monoid (..), mconcat)


newtype Prioritized a = Prioritized [(Float,a)] deriving (Eq)

instance Monoid (Prioritized a) where
        Prioritized xs `mappend` Prioritized ys = Prioritized $ f xs ys where 
                f (x@(t,_):xs) (y@(t',_):ys) 
                        | t <= t' = x : f xs (y:ys)
                        | True = y : f (x:xs) ys
        mempty = Prioritized []

newtype Time = Time Rational deriving (Num,Fractional,Ord,Eq,Show,Read,Enum)


data  Generator a = G Time Time [(Time,a)] deriving (Show,Read,Eq)

linear p w t = (t - p) / w
glinear p w (t,x) = ((t*w) + p , x)
query :: Time -> Generator a -> (Generator a,Maybe a)

query t (G p w ts) = let
                pwt = linear p w t
                ds = dropWhile ((< pwt) . fst) ts
                in case ds of
                        [] -> (G p w [],Nothing)
                        ((t',x):ds') ->  if pwt == t' then (G p w ds', Just x)
                                        else (G p w ds, Nothing)

instance Monoid a => Monoid (Generator a) where
        G p w xs `mappend` G p' w' ys = G p w $ f xs $ map (first $ (/w).(subtract p).(+p').(*w')) ys where
                f [] ys = ys
                f xs [] = xs    
                f (x@(t,xa):xs) (y@(t',ya):ys) 
                        | t == t' = (t,xa `mappend` ya) : f xs ys
                        | t < t' = x : f xs (y:ys)
                        | True = y : f (x:xs) ys
        mempty = G 0 1 []


toList (G ph w xs) = map (glinear ph w) xs

data Track a = Track (Generator Time) (Generator Time) (Generator a)
generator (Track _ _ g) = g

queryTrack :: Time -> Track a -> (Track a, Maybe a)
queryTrack t (Track gph gw (G ph w xs)) = (Track gph' gw' ga',ma) where
        (ga',ma) = query t (G ph' w' xs)
        (gph',mph) = query t gph
        (gw',mw) = query t gw
        ph' = fromMaybe ph mph
        w' = fromMaybe w mw

queryTracks :: Monoid a => Time -> [Track a] -> ([Track a], a)
queryTracks t ts = let
        (ts',mas) = unzip . map (queryTrack t) $ ts
        in (ts', mconcat . catMaybes $ mas)
       
complete :: (Monoid a , Eq a) => Time -> [Track a] -> [(Time,a)]
complete t [] = []
complete t xs = let (xs',p) = queryTracks t xs in 
        (if p == mempty then [] else [(t,p)]) ++ complete (t + 1) (filter (not . (== mempty) . generator) xs')



ph2 = G 0 8 $ zip [0..] $ concat $ repeat $ [2,-2] :: Generator Time
w2 = G 0 12 $ zip [0..] $ concat $ repeat $ [1,2,3,4] :: Generator Time

ry2 x = mconcat [G (n * 32) 8 $ zip [0,2,3] $ repeat x | n <- [0 .. 7]]
