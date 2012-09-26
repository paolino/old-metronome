module Monad where

import System.Random
import Control.Monad.State

forUntilM :: Monad m => (b -> Bool) -> [a] -> (a -> m b) -> m ([b],[a])
forUntilM t [] _ = return ([],[])
forUntilM t (x:xs) f = do
        y <- f x
        if t y then return ([y],xs) else do
                (ys,rs) <- forUntilM t xs f 
                return (y:ys, rs)

iterateM :: (Monad m, Functor m) => (a -> m a) -> a -> m [a]
iterateM f x = f x >>= \y ->  (y:) `fmap`  iterateM f y

evalRandomIO f =  newStdGen >>= return . evalState f

