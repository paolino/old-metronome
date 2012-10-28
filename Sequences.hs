module Sequences where


import Control.Arrow ((&&&), first)
import Data.List (groupBy, sortBy, mapAccumL, tails)
import Data.Ord (comparing)
import Data.Function (on)
import Data.Ratio
import Data.Monoid
import Control.Monad.Random
import Data.Fixed
import Debug.Trace
import Control.DeepSeq
import Data.Sequence (Seq,viewl, viewr, ViewL (..),ViewR (..),(<|),(|>), fromList)

rdivMod :: Double -> Double -> (Integer,Double)
rdivMod x y = let 
        z = x/y
        in (floor z, x - (fromIntegral . floor $ z) * y)

rMod x y = snd . rdivMod y $ x
rDiv x y = fst . rdivMod y $ x
quantize k = first (flip mod k . rDiv (1/fromIntegral k) )

type Sq b a = [(b,a)]

collapse :: (Ord b,Eq b) =>  ([a] -> a) -> [(b,a)] -> [(b,a)]
collapse f = map (fst . head &&& f . map snd) . groupBy ((==) `on` fst) . sortBy (comparing fst)



data Linear = Ct Double | Sh Double deriving (Show,Read) 
linear :: Linear -> Double -> Double
linear (Ct w) x = w * x 
linear (Sh w) x = x + w


 :: (Functor m ,MonadRandom m, NFData a) =>  [(Double, Linear)] -> Sq Double a -> m (Sq Double a)
fractal xs ys = do 
        l <- pick xs 
        return $ map (first $ linear l) ys



