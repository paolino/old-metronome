{-# LANGUAGE FlexibleContexts #-}

import Prelude hiding (Either (..))
import Data.List (splitAt, tails, nub , delete)


import Coo
import Cursor3
import Actions
import Corr

import System.Console.Haskeline

import Sound.OpenSoundControl
import Control.Concurrent.STM 
import Control.Monad
import Control.Concurrent
import Supercollider4
import Data.Map (empty, fromList)
import Control.Monad.Random hiding (fromList)
import Data.Maybe (catMaybes)
import Control.DeepSeq
import Control.Monad.Trans
import Control.Arrow
import Data.Array

type Index = Int


data Track = Track Int Int Int Int Action deriving (Show,Read)

track :: NoteOn -> TVar Int -> TVar Params -> TVar (Maybe (Array (Int,Int) Double)) -> TChan (Int,Tempo) -> TVar [Track] -> IO ()
track noteon tgv tps tls tc tv = track' where
        track' = do
                (n,t) <- atomically (readTChan tc) -- aspetta
                mkls <- atomically $ readTVar tls 
                case mkls of 
                        Nothing -> return ()
                        Just lcom -> do 
                                let     ((0,0),(mi,mj)) = second ((+1) *** (+1)) $ bounds lcom
                                        c l mph ph = (ph + (((n - mph) `mod` mj) `mod` l)) `mod` mj
                                        v i mph ph l = lcom ! (i `mod` mi,c l mph ph )
                                        vs i = [lcom ! (i `mod` mi ,j) | j <- [0..mj - 1]] 
                                gse <- atomically $ readTVar tgv -- seme
                                rs  <- atomically $ readTVar tv -- azioni
                                actCollect $ zipWith (\ i (Track se mph ph l x)  -> render noteon tps x  (se + gse) (c l mph ph) (v i mph ph l) (vs i) t) [0..] rs
                track'  


data Command = S | F String | Se Int | Ph Int Int | Wd Int Int | Sa String | Ld String | Set Int Int | Mph Int Int deriving (Show,Read)

d = 0.125
main = do
        bootSamples
        sync <- newTChanIO :: IO (TChan (Int,Tempo))
        modi <- newTChanIO 
        t0' <- utcr
        let     m =    floor $  t0' / d
                t0 = fromIntegral (m + 1) * d
                times = map (id &&& ((t0 +).(*d). fromIntegral)) [0..]
                z ((c,t):ts) = do 
                        sleepThreadUntil t 
                        atomically $ do 
                                join (readTChan modi) `orElse` return ()
                                writeTChan sync (c, t)
                        z ts
        w <- forkIO $ z times

        com <- newTVarIO Nothing 
        st <- newTVarIO empty
        gtv <- newTVarIO 0
        com <- newTVarIO Nothing 
        st <- newTVarIO empty

        ths <- newTVarIO []
                        
        u <- forkIO $  track scNoteOn  gtv st com sync ths

        let loop = do 
                o <- getInputLine ":> "
                case o of 
                        Nothing -> return ()
                        Just o -> 
                                case reads o of 
                                        [(Sa f,_)] -> do 
                                                liftIO $ do
                                                        t <- atomically $ readTVar ths    
                                                        l <- atomically $ readTVar com   
                                                        writeFile f $ show (t,l)
                                                loop
                                        [(Ld f,_)] -> do 
                                                liftIO $ do
                                                        (t,l) <- read `fmap` readFile f 
                                                        atomically $ writeTVar ths t
                                                        atomically $ writeTVar com l
                                                loop
                                        [(Ph i ph,_)] -> do 
                                                liftIO . atomically $writeTChan modi $ modifyTVar ths $ \xs ->
                                                        let (sx,Track se mph _ l a:dx) = splitAt i xs in sx ++ (Track se mph ph l a:dx)
                                                loop
                                                                
                                        [(Mph i mph,_)] -> do 
                                                liftIO . atomically $writeTChan modi $ modifyTVar ths $ \xs ->
                                                        let (sx,Track se _ ph l a:dx) = splitAt i xs in sx ++ (Track se mph ph l a:dx)
                                                loop
                                        [(Set i se,_)] -> do 
                                                liftIO . atomically $writeTChan modi $ modifyTVar ths $ \xs ->
                                                        let (sx,Track _ mph ph l a:dx) = splitAt i xs in sx ++ (Track se mph ph l a:dx)
                                                loop

                                        [(Wd i l,_)] -> do 
                                                liftIO . atomically $writeTChan modi $ modifyTVar ths $ \xs ->
                                                        let (sx,Track se mph ph _ a:dx) = splitAt i xs in sx ++ (Track se mph ph l a:dx)
                                                loop
                                        [(S,_)] -> return ()
                                        [(F f,_)] -> do 
                                                liftIO $ do 
                                                        x <- readFile f
                                                        let ls = lines x
                                                        let g = read $ last ls
                                                        deepseq g . atomically $ writeTChan modi $ do 
                                                                writeTVar com $ Just g
                                                loop
                                        [(Se i,_)] -> do 
                                                liftIO $ do 
                                                        atomically $ writeTVar gtv $ i
                                                loop
                                        _ -> outputStrLn "no parse" >> loop
        runInputT (defaultSettings{ historyFile = Just "Main.hist", autoAddHistory = True}) $ loop
        killThread w
        killThread u
