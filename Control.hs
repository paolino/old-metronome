module Control where


import Prelude hiding (lookup)
import Control.Monad
import Data.Map

data Control k = N Double | A k | S (Control k) (Control k) | M (Control k) (Control k) | P k (Control k) deriving (Show,Read,Ord,Eq)

resolve :: Ord k => Map k [(Double,Double)] -> Map k Double -> Control k -> Maybe Double
resolve _ _ (N x) = Just x
resolve _ m (A k) = lookup k m
resolve mz m (S c1 c2) = liftM2 (+) (resolve mz m c1) (resolve mz m c2)
resolve mz m (M c1 c2) = liftM2 (*) (resolve mz m c1) (resolve mz m c2)
resolve mz m (P k c) = do 
        v <- resolve mz m c 
        im <- lookup k mz 
        lookup' v im



lookup' x y = let 
        ys' = (\ys -> zip ((0,0):ys) ys ) y
        ys'' = dropWhile (\((x1,y1),(x2,y2)) -> x2 < x) ys'
        in case ys'' of
                [] -> Nothing
                (((x1,y1),(x2,y2)):_) -> Just $ y1 + (x - x1)/(x2 - x1) * (y2 - y1)

