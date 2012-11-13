module Actions where

import Prelude hiding (lookup, filter)
import Data.List (mapAccumL)
import Control.Monad (when, forM_, join, (>=>))
import Control.Concurrent ()
import Control.Concurrent.STM (atomically, newTVar, modifyTVar, TVar, STM, readTChan, readTVar, dupTChan, newBroadcastTChanIO, writeTChan, writeTVar)
import Sound.OpenSoundControl (sleepThreadUntil)
import Control.Monad.Random (getRandomR, Random, evalRand, mkStdGen)
import Data.Map.Strict (assocs, empty,lookup, Map, singleton, insert,filter, keys, member , (!), fromList)
import System.Random (StdGen)
                        


type Tempo = Double

type Params = Map String Double 
        
data Action
        = Play 
                Tempo -- ritardo
                String -- synth name
                [(String,String)] -- parameter resolution
                Double -- top probability border
        |  TParam 
                String -- parameter name
                Double -- max value
                Double -- min value
        | SParam 
                String -- parameter name
                [Double] -- choices for parameter values
                 deriving (Show,Read)

type NoteOn =  Tempo -> String -> Params -> IO ()
type Act = Int -> Double -> [Double] -> Tempo -> STM (IO ())

actCollect :: [STM (IO ())] -> IO ()
actCollect = join . fmap sequence_ . atomically . sequence

render :: NoteOn -> TVar Params -> Action -> Int -> Act
render noteon m (Play rit name res level) se n v _ t = do 
                        let l =  evalRand (getRandomR (0,level)) $ mkStdGen (se + n)
                        vs <- readTVar  m
                        return $ when (v >= l) $ do 
                                noteon (t + rit) name (fmap (vs !) $ filter (`member` vs) . fromList $ res) 
render _ m (TParam paramname top bottom) _ _ v _ _ = do
        modifyTVar m $ insert paramname (bottom + v * (top - bottom))
        return (return ())
render _ m (SParam paramname choices) se n v vs _ = do
                let v = evalRand ((cycle choices !!) `fmap` pickFrom (zip [0..] vs)) $ mkStdGen (se + n)
                modifyTVar m $ insert paramname v
                return (return ())

pickFrom xs = do
        let     l = sum $ map snd xs
                f s (x,y) = (s + y,(x , s + y))
        t <- getRandomR (0,l)
        return $ fst . head $ dropWhile ((<t) . snd) . snd . mapAccumL f 0 $ xs



        

