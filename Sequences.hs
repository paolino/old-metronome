module Sequences where


import Control.Arrow ((&&&), first)
import Data.List (groupBy, sortBy, mapAccumL)
import Data.Ord (comparing)
import Data.Function (on)
import Data.Ratio
import Data.Monoid
import Control.Monad.Random
import Data.Fixed

rdivMod :: Double -> Double -> (Integer,Double)
rdivMod x y = let 
        z = x/y
        in (floor z, x - (fromIntegral . floor $ z * y))

rMod x y = snd . rdivMod x $ y

type Sq b a = [(b,a)]

collapse :: (Ord b,Eq b) =>  ([a] -> a) -> [(b,a)] -> [(b,a)]
collapse f = map (fst . head &&& f . map snd) . groupBy ((==) `on` fst) . sortBy (comparing fst)


data Linear = Linear Double Double 
linear :: Linear -> Double -> Double
linear (Linear w ph) x = w * x + ph


fractal :: MonadRandom m =>  [(Double, Linear)] -> Sq Double a -> m (Sq Double a)
fractal xs ys = do
        let t = sum $ map fst xs
        z <- getRandomR (0,t)
        let ((_,x):_) = dropWhile ((<t) . fst) . snd . mapAccumL (\t (t',x) -> (t + t',(t + t',x))) 0 $ xs 
        return $  map (first $ linear x) ys
        
fractalN :: (Monad m, MonadRandom m, Functor m) => Int -> Double -> Sq Double Linear -> Sq Double a -> m (Sq Double a)
fractalN 0 k _ x = return x
fractalN n k y x = fractal y x >>= fractalN  (n -1) k y . map (first $ (snd . flip rdivMod k))

iterateN :: (Monad m, MonadRandom m, Functor m) => Int -> Double -> Sq Double Linear -> Sq Double a -> m [Sq Double a]
iterateN 0 k _ x = return [x]
iterateN n k y x = do 
        x' <- (map (first $ (snd . flip rdivMod k))) `fmap` fractal y x 
        xs' <- iterateN (n - 1) k y x'
        return $ x':xs'

fractalFromTo n m k y x = do
        x' <- fractalN n k y x
        iterateN (m - n) k y x'


quantize k x = fst $ x `rdivMod` k

qf :: (Functor m, MonadRandom m) => Double -> Int -> [(Double,Linear)] -> Sq Double a -> m (Sq Integer a)
qf k n y = fmap (map (first $ (quantize (1/k)))) . fractalN n 1 y

qfi :: (Functor m, MonadRandom m) => Double -> Int -> Int -> Sq Double Linear -> Sq Double a -> m [Sq Integer a]
qfi k n m y = fmap (map (map (first $ (quantize (1/k))))) . fractalFromTo n m 1 y
