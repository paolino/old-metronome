{-# LANGUAGE DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Rytmics where

import Data.Ratio
import Data.List
import Control.Arrow
import Data.Traversable
import Data.Foldable

data Absolute
data Relative

type R b a = (Rational, a)

data L a = L (R Relative a) | Mappend (L a) (L a) | Mix (L a) (L a) | Shift Rational (L a) | EP deriving (Functor, Show, Read, Traversable,Foldable)

mul :: Rational -> L a -> L a
mul r EP = EP
mul r (L (r',x)) = L (r'*r,x)
mul r (Mappend x y) = Mappend (r `mul` x) (r `mul` y)
mul r (Mix x y) = Mix (r `mul` x) (r `mul` y)
mul r (Shift s x) = Shift (r * s) (r `mul` x)

mix :: [R Absolute a] -> [R Absolute a] -> [R Absolute a]
mix [] x = x
mix x [] = x
mix (x@(tx,_):xs) (y@(ty,_):ys) 
        | tx <= ty = x:mix xs (y:ys)
        | otherwise = y:mix (x:xs) ys


instance Monad L where
        return x = L (1,x)
        EP >>= _ = EP
        L (t,x) >>= f = t `mul` f x
        Mappend x y >>= f = (x >>= f) `Mappend` (y >>= f)
        Mix x y >>= f = (x >>= f) `Mix` (y >>= f)
        Shift s x >>= f = Shift s (x >>= f)

-- | the Rational value is the offset after last event
data P a = P Rational [R Absolute a] deriving Show

mkP :: Rational -> L a -> P a 
mkP r (Mappend x y) = 
                let P r' xs = mkP r x 
                    P r'' ys =  mkP r' y
                in P r'' $ xs ++ ys
mkP r (Mix x y) = let 
        P rx xs = mkP r x
        P ry ys = mkP r y
        in P (max rx ry) $ mix xs ys
mkP r (Shift r' x) = mkP (r + r') x
mkP r (L x) = P (r + fst x) [(r,snd x)]
mkP r EP = P r []

many :: [R Relative a] -> L a
many [] = EP
many (x:xs) = Mappend (L x) (many xs)

