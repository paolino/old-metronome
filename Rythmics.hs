{-# LANGUAGE DeriveFunctor, DeriveTraversable, DeriveFoldable, TypeFamilies #-}

module Rythmics where

import Prelude hiding (foldr)
import Data.Ratio
import Data.List hiding (foldr)
import Control.Arrow
import Data.Traversable
import Data.Foldable

data Absolute
data Relative

type R b a = (Rational, a)

data L a = Event (R Relative a) | Mappend (L a) (L a) | Merge (L a) (L a) | Pause Rational deriving (Functor, Show, Read, Traversable,Foldable)


total :: L a -> Rational
total (Pause r) = r
total (Event (r,_)) = r
total (Mappend x y) = total x + total y
total (Merge x y) = total x + total y

normalize xs = (1/total xs) `mul` xs 

mul :: Rational -> L a -> L a
mul r (Pause r') = (Pause $ r * r')
mul r (Event (r',x)) = Event (r'*r,x)
mul r (Mappend x y) = Mappend (r `mul` x) (r `mul` y)
mul r (Merge x y) = Merge (r `mul` x) (r `mul` y)

mix :: [R Absolute a] -> [R Absolute a] -> [R Absolute a]
mix [] x = x
mix x [] = x
mix (x@(tx,_):xs) (y@(ty,_):ys) 
        | tx <= ty = x:mix xs (y:ys)
        | otherwise = y:mix (x:xs) ys


instance Monad L where
        return x = Event (1,x)
        Pause r >>= _ = Pause r
        Event (t,x) >>= f = t `mul` f x
        Mappend x y >>= f = (x >>= f) `Mappend` (y >>= f)
        Merge x y >>= f = (x >>= f) `Merge` (y >>= f)

-- | the Rational value is the offset after last event
data P a = P Rational [R Absolute a] deriving Show

mkP :: Rational -> L a -> P a 
mkP r (Mappend x y) = 
                let P r' xs = mkP r x 
                    P r'' ys =  mkP r' y
      in P r'' $ xs ++ ys
mkP r (Merge x y) = let 
        P rx xs = mkP r x
        P ry ys = mkP r y
        in P (max rx ry) $ mix xs ys
mkP r (Event x) = P (r + fst x) [(r,snd x)]
mkP r (Pause r') = P (r + r') []

class Index b where
        readL :: b -> L a -> [L a]
        modifyL :: b -> (L a -> L a) -> L a -> L a
        newL :: b -> a -> L a



