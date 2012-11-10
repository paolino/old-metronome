module Actions where

import Prelude hiding (lookup, filter)
import Data.List (mapAccumL)
import Control.Monad (when, forM_)
import Control.Concurrent ()
import Control.Concurrent.STM (atomically, newTVar, modifyTVar, TVar, STM, readTChan, readTVar, dupTChan, newBroadcastTChanIO, writeTChan, writeTVar)
import Sound.SC3 hiding (Binary,select,Linear)
import Sound.OpenSoundControl (OSC (..), Time (..),sleepThreadUntil)
import Control.Monad.Random (getRandomR, Random, evalRand, mkStdGen)
import Data.Map.Strict (assocs, empty,lookup, Map, singleton, insert,filter, keys, member , (!), fromList)
                        
import Schedule
import Cursor3 hiding (insert)




type Params = Map String Double 
        

writeParams m parname val = atomically $ modifyTVar m $ insert parname val
                        

data Play = Play 
        Int -- seed
        Int -- steps
        Double -- ritardo
        String -- synth name
        [(String,String)] -- parameter resolution
        Double -- top probability border


type NoteOn =  Double -> String -> Params -> IO ()
type Parameters = TVar (Map String (TVar Params))
        

renderPlay :: TVar Params -> NoteOn -> Play -> Render Revol
renderPlay m noteon (Play see l rit name res level) start end (stat) = do
                let     ts = [start , start + (end - start)/fromIntegral l .. ] 
                forM_ (zip3 [0..] stat ts) $ \(n,v,t) -> do 
                        sleepThreadUntil t 
                        l <- getRandomR (0,level)
                        when (v >= l) $ do 
                                vs <- atomically (readTVar  m) 
                                noteon (t + rit) name (fmap (vs !) $ filter (`member` vs) . fromList $ res) 

                
data TParam = TParam 
        Int -- steps
        String -- parameter name
        Double -- max value
        Double -- min value

renderTParam :: TVar Params -> TParam -> Render Revol
renderTParam m (TParam l paramname top bottom) start end (stat) = do
                let     ts = [start , start + (end - start)/fromIntegral l .. ] 
                forM_ (zip3 [0..] stat ts) $ \(n,v,t) -> do 
                        sleepThreadUntil t 
                        writeParams m paramname (bottom + v * (top - bottom))

data SParam = SParam 
        Int -- seed
        Int -- validity steps
        String -- parameter name
        (Map Int Double) -- choices for parameter values


pickFrom :: [(a, Double)] -> IO a
pickFrom xs = do
        let     l = sum $ map snd xs
                f s (x,y) = (s + y,(x , s + y))
        t <- getRandomR (0,l)
        return $ fst . head $ dropWhile ((<t) . snd) . snd . mapAccumL f 0 $ xs
         
renderSParam :: TVar Params -> SParam -> Render Revol
renderSParam m (SParam see l paramname choices) start end (stat) = do
        let     ts = take l $ zip [0..] $ [start , start + (end - start)/fromIntegral l .. ] 
        forM_ ts $ \(i,t) -> do 
                sleepThreadUntil t 
                rs <-  flip lookup choices `fmap` pickFrom (zip [0..] stat) 
                case rs of
                        Nothing -> return ()
                        Just v -> writeParams m paramname  v




        

