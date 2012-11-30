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
import Data.Maybe (catMaybes, fromMaybe)
import Control                        


type Tempo = Double

type Params a = Map String a
     

data Action
        = Play 
                (Control String) -- ritardo
                (Either String  Int) -- synth name or instance
                [(String,Control String)] -- parameter resolution
                (Control String) -- top probability border
        |  TParam 
                String -- parameter name
                (Control String) -- max value
                (Control String) -- min value
        | SParam 
                String -- parameter name
                [(Control String,Double)] -- choices for parameter values
                 deriving (Show,Read,Eq)

instance Ord Action where
        compare (Play _ _ _ c) (Play _ _ _ c') = compare c' c
        compare (Play _ _ _ c) _ = LT
        compare _ (Play _ _ _ c)  = GT
        compare (TParam c _ _) (SParam c' _) = compare c c' 
        compare (SParam c _ ) (TParam c' _ _)  = compare c c' 
        compare (TParam c _ _) (TParam c' _ _) = compare c c' 
        compare (SParam c _) (SParam c' _)  = compare c c' 
         
fromControl mp mv x y = fromMaybe x $ resolve mp mv y 
type NoteOn =  Tempo -> Either String Int -> Params Double -> IO ()


type Act = Int -> Double -> [Double] -> Tempo -> STM (IO ())

actCollect :: [STM (IO ())] -> IO ()
actCollect = join . fmap sequence_ . atomically . sequence


render :: NoteOn -> TVar (Params Double)  -> TVar (Map String [(Double,Double)]) -> Action -> Act
render noteon tmv tmp (Play rit name res level) n v _ t = do 
                        mv <- readTVar tmv
                        mp <- readTVar tmp   
                        return . when (v >= fromControl mp mv 0.5 level) $ noteon (t + fromControl mp mv 1 rit) name . fromList . catMaybes . map (\(k,x) -> fmap ((,) k) $ resolve mp mv  x) $ res 
render _ tmv tmp  (TParam paramname top bottom) _  v _ _ = do
        mv <- readTVar tmv
        mp <- readTVar tmp        
        modifyTVar tmv $ insert paramname (fromControl mp mv 0.5 bottom + v * (fromControl mp mv 0.5 top - fromControl mp mv 0.5 bottom))
        return (return ())
render _ tmv tmp  (SParam paramname choices) n v vs _ = do
        mv <- readTVar tmv
        mp <- readTVar tmp        
        when (not $ null choices) $ do
                let     l' = sum $ map snd choices
                        z = pickFrom choices (v/maximum vs*l')
                modifyTVar tmv . insert paramname $ fromControl mp mv 0.5 z
        return (return ())

heado [] = error "ahi"
heado x = head x
pickFrom xs t =  let f s (x,y) = (s + y,(x , s + y))
        in fst . heado $ dropWhile ((<t) . snd) . snd . mapAccumL f 0 $ xs

pickFromM xs l = do
        t <- getRandomR (0,l)
        return $ pickFrom xs t   




        

