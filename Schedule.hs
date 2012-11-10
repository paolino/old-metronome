{-# LANGUAGE TemplateHaskell, FlexibleContexts, TypeFamilies, MultiParamTypeClasses #-}
module  Schedule where


import Prelude hiding (lookup,Either (..))

import Data.List (maximumBy)
import Data.Ord (comparing)
import Control.Arrow ((&&&))
import Control.Concurrent.STM
import Control.Concurrent (forkIO, killThread)
import Control.Monad (forever, join)
import Sound.OpenSoundControl (sleepThreadUntil,utcr)


class Cursor b m  where
	variate :: Variator b m -> b m -> b m
	point :: b m -> (m,Goodness b m)

type family Variator (b :: * -> *) m
type family Goodness (b :: * -> *) m

type Tempo = Double

type Render m = Tempo -> Tempo -> m -> IO ()

data Bar b m =  Bar (Variator b m) (b m) [Render m]

data Track b m = Track Int [Tempo] (Bar b m) 

best :: (Cursor b m, Ord (Goodness b m)) => Int -> Variator b m -> b m -> (b m ,(m, Goodness b m))
best k l x = let
        ys = take k . tail . iterate (variate l) $ x
        in (last ys, maximumBy (comparing snd) . map point $ ys)


track :: (Show (Goodness b m), Ord (Goodness b m), Cursor b m) => TVar (Track b m) -> IO ()
track tv = track' [] Nothing where
        track' thrs mpv = join . atomically $ do
                Track k ts (Bar va x rs) <- readTVar tv
                case ts of
                        (t:t':ts) -> do 
                                let     (y,(p,v)) = best k va x
                                writeTVar tv (Track k (t':ts) $ Bar va y rs)
                                return $ do 
                                        let (p'',v'') = maybe (p,v) (\(p',v') -> if v > v' then (p,v) else (p',v')) mpv
                                        tn <- utcr
                                        sleepThreadUntil t 
                                        mapM_ killThread thrs
                                        thrs' <- if  t > tn then mapM (\r -> forkIO $ r t t' p) rs else return []
                                        track' thrs' $ Just (p'',v'')
                        _ -> return (return ())


