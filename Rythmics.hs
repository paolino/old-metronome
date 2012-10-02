{-# LANGUAGE FlexibleInstances, GADTs, ExistentialQuantification #-}

module Rythmics where

import Prelude hiding (foldr)
import Data.Ratio
import Data.List hiding (foldr)
import Control.Arrow
import Data.Traversable
import Data.Foldable
import Data.Monoid

data Absolute
data Relative

type Event a = (Integer,a)
data R b a where 
        Events :: [Event a] -> R Absolute a
        Pattern :: Render c => c -> a -> R Relative a

instance Functor (R Absolute) where
        f `fmap` (Events xs) = Events $ second f `fmap` xs
instance Functor (R Relative) where
        f `fmap` (Pattern c x) = Pattern c (f x)
        
class Render c where
        mkEvents :: c -> a -> R Absolute a


instance Monoid (R Absolute a) where
        mempty = Events []
        mappend (Events xs) (Events ys) = Events $ mix xs ys


mix :: [Event a] -> [Event a] -> [Event a]
mix [] x = x
mix x [] = x
mix (x@(tx,_):xs) (y@(ty,_):ys) 
        | tx <= ty = x:mix xs (y:ys)
        | otherwise = y:mix (x:xs) ys




