{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

import Data.Map hiding (map , foldr)
import Data.List (mapAccumL)
import Control.Arrow (second)


type Compute a = a -> a -> LinkValue a

type family Id a 

type Population a = Map (Id a) a

type family LinkValue a 

data Link a = Link (Id a) (Id a)

type Ranking a = Map (LinkValue a) [Link a]

new :: (Ord (LinkValue a), Ord (Id a) , Enum (Id a)) => Compute a -> a -> Population a -> (Population a, Ranking a)
new f x p =  let
        kvs = assocs p
        kx = succ (last $ map fst kvs)
        in (insert kx x p, fromListWith (++) (map (\(ky,y) -> (f x y, [Link kx ky])) kvs))

takeBest :: (Ord (LinkValue a)) => Int -> Ranking a -> Ranking a
takeBest n p = fromList . map snd . takeWhile ((<=n) . fst) . snd . mapAccumL (\m (k,xs) -> (m + length xs,(m,(k,xs)))) 0 $ toDescList p

step :: (Ord (LinkValue a), Ord (Id a), Enum (Id a)) => Int -> Compute a -> Population a -> Ranking a -> [a] -> (Population a, Ranking a)
step n f p r = second (takeBest n) . foldr (\x (p,r) -> second (unionWith (++) r) $ new f x p) (p,r) 




