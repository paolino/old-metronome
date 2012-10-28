{-# LANGUAGE DeriveFunctor,DeriveFoldable,DeriveTraversable #-}

module Algorythm where

import Prelude hiding (foldr,mapM,foldr1)
import Control.Concurrent.STM (TVar, atomically,STM,modifyTVar,newTVar, readTVar)
import Data.Map (empty, Map, adjust, fromList, (!))
import Data.Monoid (Monoid (..))
import Control.Arrow (first,second,(***),(&&&))
import Sound.OpenSoundControl (sleepThreadUntil,utcr)
import Control.Monad (forM_, forM)
import Data.Foldable (Foldable,foldr,toList,foldr1)
import Data.List (groupBy, sortBy)
import Data.Ord (comparing)
import Data.Function (on)
import Data.Traversable (Traversable,mapM)
import Data.Functor ((<$>))
import Debug.Trace
import Sequences


type Param = (String,Double)

data Track a = Q Integer Integer (Sq Integer a) deriving (Read,Show,Functor)

type Action = [((String,[Param]),[Param])]

collapseA :: Action -> Action -> Action
collapseA xs ys = collapse (collapse sum . concat) $ xs ++ ys 



quantize' :: Integer -> Track Action -> Sq Integer Action
quantize' i (Q w ph x) = let
        (n,r) = (i - ph) `divMod` w
        z = (fromIntegral n + 1) * w + ph 
        
        in map (first (+z)) $ x             

type Render = Action -> Double -> IO ()

render  :: Render -> Double -> Track Action -> IO ()
render r d xs = do
        t0 <- utcr
        let     n = floor $ t0 / d
        forM_ (quantize' n xs) $ \(i,a) -> do
                let t1 = fromIntegral i * d
                sleepThreadUntil t1
                r a t1
{-
serialize :: [Track V] -> IO ([(String,Map String Double)],[Track String])
serialize xs = do
        xs' <- atomically $ mapM (mapM (\(s,v) -> (,) s `fmap` readTVar v)) xs
        let vs' = map head . groupBy ((==) `on` fst) . sortBy (comparing fst) $ concatMap toList xs'
        return (vs', map (fmap fst) xs')
        
recover :: ([(String,Map String Double)],[Track String]) -> IO ([V],[Track V])
recover (vs,x) = do
        vs' <- atomically $ mapM (\(s,v) -> (,) s `fmap` newTVar v) $ vs
        return $ (vs',map (fmap (\k -> (k,fromList vs' ! k))) x)
-}
{-

data Construct a = Complete (String, Play a) | Partial (String, Play a -> Construct a)

mkT s = mkT' []
        mkT rs [] = Complete (s, T $ reverse rs)
        mkT rs (n:ns)  = Partial $ ("what to play at " ++ show n,\x -> mkT ((n,x):rs) ns)

mkQ s w p = Partial ("what to quantize", Complete $ (s,Q w p))
-}








