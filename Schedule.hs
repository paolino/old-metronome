{-# LANGUAGE TemplateHaskell, FlexibleContexts, TypeFamilies, MultiParamTypeClasses #-}
module  Schedule where


import Prelude hiding (lookup,Either (..))

import Data.List (maximumBy)
import Data.Ord (comparing)
import Control.Arrow ((&&&), second)
import Control.Concurrent.STM
import Control.Concurrent (forkIO, killThread)
import Control.Monad (forever, join, when)
import Sound.OpenSoundControl (sleepThreadUntil,utcr)


class Cursor b m  where
	variate :: Variator b m -> b m -> b m
	point :: b m -> (m,[b m] -> Goodness b m)

type family Variator (b :: * -> *) m
type family Goodness (b :: * -> *) m

type Tempo = Double

type Render m = Tempo -> Tempo -> m -> IO ()

data Bar b m =  Bar (Variator b m) (b m) (b m) [Render m]

data Track b m = Track Int [Tempo] (Bar b m) 

load :: (Cursor b m) => Track b m -> b m
load (Track k ts (Bar va xp x rs)) = xp

track :: (Show (Goodness b m), Ord (Goodness b m), Cursor b m) => [TVar (Track b m)] -> TVar (Track b m) -> IO ()
track cs tv = track' [] where
        track' thrs = join . atomically $ do
                Track k ts (Bar va xp x rs) <- readTVar tv
                case ts of
                        (t:t':ts) -> do 
                                cs' <- mapM (fmap load . readTVar) cs
                                let     y = variate va x
                                        (p,v) = second ($ cs') $ point y
                                        (p',v')= second ($ cs') $ point xp
                                        (cond,(p'',v'')) = if v >= v' then (True,(p,v)) else (False,(p',v'))
                                
                                writeTVar tv (Track k (t':ts) $ Bar va (if cond then y else xp) y rs)
                                return $ do 
                                        when cond $ print v  
                                        tn <- utcr
                                        sleepThreadUntil t 
                                        mapM_ killThread thrs
                                        thrs' <- if  t > tn then mapM (\r -> forkIO $ r t t' p) rs else return []
                                        track' thrs' 
                        _ -> return (return ())


