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
import Data.List

type Index = Int

data Track = Track Int Int Int Int Action deriving (Show,Read)

track :: NoteOn -> TVar Params -> TVar (Maybe (Array (Int,Int) Double)) -> TChan (Int,Tempo) -> TVar [Track] -> IO ()
track noteon tps tls tc tv = track' where
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
                                rs  <- atomically $ readTVar tv -- azioni
                                actCollect $ zipWith (\ i (Track se mph ph l x)  -> render noteon tps x se (c l mph ph) (v i mph ph l) (vs i) t) [0..] rs
                track'  


data Reg = O Int | Lo [Int] | A deriving (Show,Read)

data Rep = On Int | Mu [Int]

data Command = S | F String | Se Int | Ph Reg Int | Wd Reg Int | Sa String | Ld String | Set Reg Int | Mph Reg Int | Sc [Command] | R Bool 
                | Uc Int Int | Pr Reg Double deriving (Show,Read)

select :: [(Int,a)] -> Reg -> (a -> a) -> [(Int,a)]
select xs A f = map (second f) xs
select xs (O x) f =  select xs (Lo [x]) f
select [] (Lo ms) f = []
select ((i,x):xs) (Lo ms) f = (i,if i `elem` ms then f x else x):select xs (Lo ms) f
        


d = 0.125
main = do
        bootSamples
        sync <- newTChanIO :: IO (TChan (Int,Tempo))
        modi <- newTChanIO 
        repeat <- newTVarIO False
        t0' <- utcr
        let     m =    floor $  t0' / d
                t0 = fromIntegral (m + 1) * d
                times = map (id &&& ((t0 +).(*d). fromIntegral)) [0..]
                z ((c,t):ts) = do 
                        sleepThreadUntil t 
                        atomically $ do 
                                f <- (fmap Just $ readTChan modi) `orElse` return Nothing
                                case f of
                                        Just g -> do 
                                                g 
                                                b <- readTVar repeat 
                                                when b $  writeTChan modi g
                                        Nothing -> return ()
                                
                                writeTChan sync (c, t)
                        z ts
        w <- forkIO $ z times

        com <- newTVarIO Nothing 
        st <- newTVarIO empty
        com <- newTVarIO Nothing 
        st <- newTVarIO empty

        ths <- newTVarIO []
                        
        u <- forkIO $  track scNoteOn  st com sync ths

        let     loop = do 
                        o <- getInputLine ":> "
                        case o of 
                                Nothing -> return ()
                                Just o -> case reads o of 
                                        [(c,_)] -> liftIO (resolve c) >> loop
                                        _ -> outputStrLn "garbled or unknown command" >> loop
        
                resolve (Sa f) =  void . forkIO $ do
                                        t <- atomically $ readTVar ths    
                                        l <- atomically $ readTVar com    
                                        writeFile f $ show (t,l)
                resolve (Ld f) =  do
                                        (t,l) <- read `fmap` readFile f 
                                        void . forkIO . deepseq l . atomically .  writeTChan modi $ do 
                                                writeTVar ths t
                                                writeTVar com l
                resolve (Ph i ph) =   atomically . writeTChan modi . modifyTVar ths $ \xs -> 
                                        map snd . select (zip [0..] xs) i $ \(Track se mph _ l a) -> Track se mph ph l a
                                                
                resolve (Mph i mph) =  atomically . writeTChan modi . modifyTVar ths $ \xs ->
                                        map snd . select (zip [0..] xs) i $ \(Track se _ ph l a) -> Track se mph ph l a
                resolve (Set i se) =  atomically . writeTChan modi $ modifyTVar ths $ \xs ->
                                        map snd . select (zip [0..] xs) i $ \(Track _ mph ph l a) -> Track se mph ph l a

                resolve (Wd i l) =   atomically . writeTChan modi $ modifyTVar ths $ \xs ->
                                        map snd . select (zip [0..] xs) i $ \(Track se mph ph _ a) -> Track se mph ph l a
                resolve (Pr i v) =   atomically . writeTChan modi $ modifyTVar ths $ \xs ->
                                        map snd . select (zip [0..] xs) i $ f where
                                        f (Track se mph ph l a) = Track se mph ph l a' where
                                                a' = case a of 
                                                        Play dt na as _ -> Play dt na as v
                                                        x -> x 
                resolve (S) = return ()
                resolve (Uc i j) = do  
                                        l <- atomically $ readTVar com
                                        t <- atomically $ readTVar ths
                                        case l of 
                                                Nothing -> return ()
                                                Just a -> let
                                                        Track se mph ph l x = t !! i
                                                        ((0,0),(mi,mj)) = second ((+1) *** (+1)) $ bounds a
                                                        vs i = [a ! (i `mod` mi ,j) | j <- [0..mj - 1]] 
                                                        nj = badcorr (take l . drop ph . cycle $ vs i) (vs j)
                                                        in resolve (Ph (O j) nj)
                                        
                resolve (R t) =  atomically . writeTVar repeat $ t
                resolve (F f) =  do 
                                        x <- readFile f
                                        let ls = lines x
                                        let g = read $ last ls
                                        deepseq g . atomically $ writeTChan modi $ do 
                                                writeTVar com $ Just g
        runInputT (defaultSettings{ historyFile = Just "Main.hist", autoAddHistory = True}) $ loop
        killThread w
        killThread u
