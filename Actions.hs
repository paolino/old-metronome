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
import Data.Maybe (catMaybes)
                        


type Tempo = Double

type Params a = Map String a
       
data Resolve = Mul Double Resolve | Value String | Project String  | Based String String  deriving (Read,Show)    

data Action
        = Play 
                Tempo -- ritardo
                (Either String Int) -- synth name or instance
                [(String,Resolve)] -- parameter resolution
                Double -- top probability border
        |  TParam 
                String -- parameter name
                Double -- max value
                Double -- min value
        | SParam 
                String -- parameter name
                [(Double,Double)] -- choices for parameter values
                 deriving (Show,Read)
        
type NoteOn =  Tempo -> Either String Int -> Params Double -> IO ()
type Act = Int -> Double -> [Double] -> Tempo -> STM (IO ())

actCollect :: [STM (IO ())] -> IO ()
actCollect = join . fmap sequence_ . atomically . sequence

lookup' x y = let 
        ys' = (\ys -> zip ((0,0):ys) ys ) $ assocs y
        ys'' = dropWhile (\((x1,y1),(x2,y2)) -> x2 < x) ys'
        in case ys'' of
                [] -> Nothing
                (((x1,y1),(x2,y2)):_) -> Just $ y1 + (x - x1)/(x2 - x1) * (y2 - y1)

render :: NoteOn -> TVar (Params Double)  -> TVar (Map Double Double) -> Action -> Act
render noteon mv  mp (Play rit name res level) n v _ t = do 
                        vs <- readTVar mv
                        ps <- readTVar mp        
                        let     f (Mul k v) = fmap (*k) $ f v
                                f (Value v) = lookup v vs 
                                f (Project i) = lookup i vs >>= flip lookup ps
                                f (Based i1 i2) = lookup i1 vs >>= \b -> lookup i2 vs >>= \o -> lookup' (b + o) ps
                        return . when (v >= level) $ noteon (t + rit) name . fromList . catMaybes . map (\(k,x) -> fmap ((,) k) $ f x) $ res 
render _ mv _  (TParam paramname top bottom) _  v _ _ = do
        modifyTVar mv $ insert paramname (bottom + v * (top - bottom))
        return (return ())
render _ mv _  (SParam paramname choices) n v vs _ = do
                let     l' = sum $ map snd choices
                        z = pickFrom choices (v/maximum vs*l')
                modifyTVar mv . insert paramname $ z
                return (return ())

pickFrom xs t =  let f s (x,y) = (s + y,(x , s + y))
        in fst . head $ dropWhile ((<t) . snd) . snd . mapAccumL f 0 $ xs

pickFromM xs l = do
        t <- getRandomR (0,l)
        return $ pickFrom xs t   




        

