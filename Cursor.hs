{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, TemplateHaskell, TypeFamilies, NoMonomorphismRestriction #-}
module Cursor where

import Prelude hiding (Either (..),minimum, maximum)
import Data.Map.Strict (Map, unionWith, fromList, elems, assocs, size, fromListWith) 
import Control.Monad ((>=>))
import Data.List (mapAccumL, sortBy)
import Control.Arrow (second)
import Control.Monad.Random (getRandomR, MonadRandom, evalRand,evalRandIO)
import Data.Foldable (minimum, maximum)
-- import Data.Lens.Lazy
-- import Data.Lens.Template
import System.Random (mkStdGen)
import Data.Ord (comparing)
import Corr
import Schedule
import Control.DeepSeq

class Box b a where
	insert :: a -> b -> b
	delete :: a -> b -> b

slide :: Box b a => a -> a -> b -> b
slide x y = insert y . delete x




data Zipper a =  Z a [a]
mkZ :: NFData a => [a] -> Zipper a
mkZ (x:xs) =  Z x xs

data Dir = Right | Still deriving (Show,Read)

type instance Variator Zipper a = Dir

right :: (Cursor b m, Variator b m ~ Dir) => b m -> b m
right = variate Right

type instance Goodness Zipper a = ()

instance NFData a => Cursor Zipper  a where
        variate Right z@(Z _ []) = z
        variate Right (Z _ (z:zs)) = Z z zs
	variate Still z = z
        point (Z x _) = (x,())

data Stat a b = S (Zipper a) b (Zipper a)

mkS :: (NFData b, Box b a, NFData a) => [a] -> Int -> b -> Stat a b
mkS xs n y = (!!n) . iterate openR $ S (mkZ xs) y (mkZ xs) 

openR :: (NFData b, NFData a, Box b a) => Stat a b -> Stat a b
openR (S xs y zs) =  S xs (insert (fst $ point zs) y) (right zs) 



trace ::  Int -> Revol -> [Proj] -> [Revol]
trace n = scanl (\xs y -> toEvol n . map (apply y) . fromEvol $ xs)

data Proj = Sh Double | Re Double deriving (Show,Read)

apply :: Proj -> Double -> Double
apply (Sh x) ~y = let 
	z = x + y 
	n = floor z
	in z - fromIntegral n
apply (Re x) ~y = let 
	z = x * y 
	n = floor z
	in z - fromIntegral n

type instance Variator (Stat Revol) Revol = Dir
type instance Goodness (Stat Revol) Revol = Integer

instance Cursor (Stat Revol) Revol where
	variate Right (S xs y zs) = S (right xs) (slide (fst $ point xs) (fst $ point zs) y) (right zs) 
        variate Still x = x
	point (S xs y zs) = let v = val . L (size y)  . map fromIntegral . elems . fmap (sum . elems) $ y in (y,v)



instance Box Int Int where
	insert x y = x + y 
	delete x y = subtract x y 

type Group = Map Double Int

instance (Ord b , Box c c, NFData c, NFData b) => Box (Map b c) (Map b c) where
	insert x y = let z = unionWith insert x y in z
	delete x y = let z = unionWith delete x y in z 

type Revol = Map Int Group


fromEvol :: Revol -> [Double]
fromEvol =  elems >=> assocs >=> \(x,n) -> replicate n x
 
flat :: Int -> Int -> Revol
flat n m  = fromList [(i,fromList [(d,1) 
		| d <- take m . map (/fromIntegral n) $ 
			[fromIntegral i, fromIntegral i + 1/fromIntegral m ..]])
			| i <- [0 .. n - 1]]

toEvol :: Int -> [Double] -> Revol 
toEvol n vs = z  where
        z = fromList . map (second $ \ ds -> fromListWith (+) . zip ds $ repeat 1) . snd . mapAccumL f vs $ [0..n - 1] 
	f vs i = (ts,(i,rs)) where
		(rs,ts) = break (>= fromIntegral (i + 1)/ fromIntegral n) vs


mkST :: Int -> Int -> Int -> [Proj] -> Stat Revol Revol
mkST z n m f =  mkS (trace n (flat n m) f) z (flat n 0)

dist :: Revol -> Map Int Double
dist = normalize . fmap (sum . elems)


normalize :: Map Int Int -> Map Int Double
normalize m = fmap ((/ fromIntegral (maximum m - minimum m)) . fromIntegral .  subtract  (minimum m)) m  
data Mark = Mark Int [(Double,Proj)] deriving (Show,Read)

data Fractal = Fractal Mark  
        Int -- Opening
        Int -- points
        Int -- buckets
        Int -- position
        deriving (Show,Read)

        
pick :: (Monad m, MonadRandom m, Functor m ) => [(Double,a)] -> m a
pick xs = do
        let t = sum $ map fst xs
        z <- getRandomR (0,t)
        let ((_,x):_) = dropWhile ((<z) . fst) . snd . mapAccumL (\t (t',x) -> (t + t',(t + t',x))) 0 $ xs 
        return x


fractal :: (Functor m, MonadRandom m) => [(Double,Proj)] -> m [Proj]
fractal xs = sequence $ repeat (pick xs) 

mkFractal :: Fractal -> Stat Revol Revol
mkFractal (Fractal (Mark se pa) op po bu pos) = (!! pos) . iterate right $ mkST op bu po $ evalRand (fractal pa) $ mkStdGen se


        
        
{-
mkSF :: Int -> Int -> Int -> [(Double,Proj)] -> IO (Stat Revol Revol)
mkSF z n m xs = mkST z n m `fmap` evalRandIO (fractal xs)

mkSFSeed :: Int -> Int -> Int -> Int -> [(Double,Proj)] -> Stat Revol Revol
mkSFSeed s z n m xs = mkST z n m $ evalRand (fractal xs) $ mkStdGen s
-}
