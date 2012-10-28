{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module Cursor where

import Data.Map (Map, unionWith, fromList, elems, assocs, size) 
import Control.Monad ((>=>))
import Data.List (mapAccumL)
import Control.Arrow (second)
import Control.Monad.Random (getRandomR, MonadRandom, evalRandIO)


class Box b a where
	insert :: a -> b -> b
	delete :: a -> b -> b

slide :: Box b a => a -> a -> b -> b
slide x y = insert y . delete x

class Cursor b a where
	left :: b a -> b a
	right :: b a -> b a
	point :: b a -> a

data Zipper a =  Z [a] a [a]
mkZ :: [a] -> Zipper a
mkZ (x:xs) = Z  [] x xs

instance Cursor Zipper a where
	left z@(Z [] _ _) = z
	left (Z (x:xs) y zs) = Z xs x (y:zs)
	right z@(Z _ _ []) = z
	right (Z xs y (z:zs)) = Z (y:xs) z zs
	point (Z _ x _) = x

data Stat a b = S (Zipper a) b (Zipper a)

mkS :: Box b a => [a] -> Int -> b -> Stat a b
mkS xs n y = (!!n) . iterate openR $ S (mkZ xs) y (mkZ xs) 

openR (S xs y zs) =  S xs (insert (point zs) y) (right zs) 
openL (S xs y zs) =  S (left xs) (insert (point xs) y) zs 
closeL (S xs y zs) =  S (right xs) (delete (point xs) y) zs 
closeR (S xs y zs) =  S xs (delete (point zs) y) (left zs) 

instance Box b a => Cursor (Stat a) b where
	left (S xs y zs) = S (left xs) (slide (point zs) (point xs) y) (left zs) 
	right (S xs y zs) = S (right xs) (slide (point xs) (point zs) y) (right zs) 
	point (S xs y zs) = y 



instance Box Int Int where
	insert = (+)
	delete = subtract

type Group = Map Double Int

instance (Ord b , Box c c) => Box (Map b c) (Map b c) where
	insert = unionWith insert
	delete = unionWith delete

type Revol = Map Int Group


fromEvol :: Revol -> [Double]
fromEvol =  elems >=> assocs >=> \(x,n) -> replicate n x
 
flat :: Int -> Int -> Revol
flat n m  = fromList [(i,fromList [(d,1) 
		| d <- take m . map (/fromIntegral n) $ 
			[fromIntegral i, fromIntegral i + 1/fromIntegral m ..]])
			| i <- [0 .. n - 1]]

toEvol :: Int -> [Double] -> Revol 
toEvol n vs = fromList . map (second $ \ ds -> fromList . zip ds $ repeat 1) . snd . mapAccumL f vs $ [0..n - 1] where
	f vs i = (ts,(i,rs)) where
		(rs,ts) = break (>= fromIntegral (i + 1)/ fromIntegral n) vs

data Proj = Sh Double | Re Double

apply :: Proj -> Double -> Double
apply (Sh x) y = let 
	z = x + y 
	n = floor z
	in z - fromIntegral n
apply (Re x) y = let 
	z = x * y 
	n = floor z
	in z - fromIntegral n

project :: Int -> Revol -> Proj -> Revol
project n r p = toEvol n $ map (apply p) $ fromEvol r


trace :: Int -> Revol -> [Proj] -> [Revol]
trace n = scanl (project n)

mkST :: Int -> Int -> [Proj] -> Stat Revol Revol
mkST n m f =  mkS (trace n (flat n m) f) 20 (flat n 0)

dist :: Revol -> Map Int Int
dist = fmap (sum . elems)


        
pick :: (Monad m, MonadRandom m, Functor m ) => [(Double,a)] -> m a
pick xs = do
        let t = sum $ map fst xs
        z <- getRandomR (0,t)
        let ((_,x):_) = dropWhile ((<z) . fst) . snd . mapAccumL (\t (t',x) -> (t + t',(t + t',x))) 0 $ xs 
        return x


fractal :: [(Double,Proj)] -> IO [Proj]
fractal xs = evalRandIO . sequence $ repeat (pick xs) 

mkSF :: Int -> Int -> [(Double,Proj)] -> IO (Stat Revol Revol)
mkSF n m xs = mkST n m `fmap` fractal xs


