{-# LANGUAGE TemplateHaskell #-}
module  Schedule where

import  Cursor (Revol, point, dist, Stat)

import Prelude hiding (lookup)

import Data.Map (Map,assocs,(!),lookup,insert,empty,size)
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad (forever)
import Control.Arrow ((***))
import Control.Monad.Random (getRandomR)
import Data.Maybe (catMaybes)
import Data.Lens.Lazy
import Data.Lens.Template

import Sound.OpenSoundControl (OSC (..), Time (..),utcr, sleepThreadUntil )

type Assoc = (String,String)

type Prob = Int

data Control = Play String Prob [Assoc] | Parameter Double Double String

type State = Map String Double

type Tempo = Double

type Synth = Tempo -> String -> [(String,Double)] -> IO ()

data Track = Track {
        _shift :: Tempo,
        _bar :: Tempo,
        _operation :: Control,
        _pattern :: Stat Revol Revol
        }

$(makeLens ''Track)



lookupAssoc ms (x,y) = fmap ((,) y ) . lookup x $ ms


prob :: Prob -> Int -> IO () -> IO ()
prob l v f = do
        x <- getRandomR (0,l)
        if v > x then f else return ()

sync ph d = do
        t <- utcr
        let l = floor $ (t - ph) / d
        return $ fromIntegral (l + 1) * d + ph

forkTrack :: Synth -> TVar State -> TVar Track -> IO ThreadId
forkTrack sy st tv = 
        let     f t v (Play s p ts) = do
                    vs <- atomically $ readTVar st
                    let rs = catMaybes $ map (lookupAssoc vs) ts
                    prob p v $ sy t s rs
                f _ v (Parameter m sc s) = atomically $ modifyTVar st $ Data.Map.insert s (min m $ sc * fromIntegral v)
        in  forkIO . forever $ do
                Track dt di ct sr <- atomically $ readTVar tv
                let m = dist . point $ sr
                t0 <- sync dt (di * fromIntegral (size m))
                mapM_ (\(t,v) -> sleepThreadUntil (fromIntegral t * di + t0)  >> f (fromIntegral t * di + t0) v ct) $ assocs m 



