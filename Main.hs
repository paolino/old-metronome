{-# LANGUAGE FlexibleContexts #-}

import Prelude hiding (Either (..))
import Data.List (splitAt, tails, nub , delete)


import System.Random.Shuffle
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
import Data.Map (empty, fromList,Map,insert)
import Control.Monad.Random hiding (fromList)
import Data.Maybe (catMaybes)
import Control.DeepSeq
import Control.Monad.Trans
import Control.Arrow
import Data.Array
import Data.Maybe (fromMaybe)
import Data.Function (on)

import Data.List hiding (insert)
import Data.Ord (comparing)
import Debug.Trace
type Index = Int

data Track = Track Int Int Int Int Action deriving (Show,Read)

track :: NoteOn -> TVar (Params Double)  -> TVar (Map Double Double) -> TVar (Maybe (Array (Int,Int) Double)) -> TChan (Int,Tempo) -> TVar [Track] -> IO ()
track noteon tps tpp tls tc tv = track' where
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
                                actCollect $ map (\(Track i mph ph l x)  -> render noteon tps tpp x (c l mph ph) (v i mph ph l) (vs i) t) rs
                track'  


data Reg = O Int | Lo [Int] | A deriving (Show,Read)

data Rep = On Int | Mu [Int]

data Command = S | F String | Se Int | Ph Reg Int | Wd Reg Int | Sa String | Ld String | Set Reg Int | Mph Reg Int | Sc [Command] | R Int 
                | Uc Reg Int | Pr Reg Double | As Int | Rs | Sp String Double | Ac Reg Action | T Reg Track | M Int | N (Maybe Int)| D Int  deriving (Show,Read)

select :: [(Int,a)] -> Reg -> (a -> a) -> [(Int,a)]
select xs A f = map (second f) xs
select xs (O x) f =  select xs (Lo [x]) f
select [] (Lo ms) f = []
select ((i,x):xs) (Lo ms) f = (i,if i `elem` ms then f x else x):select xs (Lo ms) f
        
dolcezza :: Num a => [a] -> a
dolcezza xs = sum . zipWith (\x y -> abs (x - y)) xs $ tail $ cycle xs

d = 0.120
main = do
        boot
        sync <- newTChanIO :: IO (TChan (Int,Tempo))
        modi <- newTChanIO 
        repeat <- newTVarIO 1
        t0' <- utcr
        let     m =    floor $  t0' / d
                t0 = fromIntegral (m + 1) * d
                times = map (id &&& ((t0 +).(*d). fromIntegral)) [0..]
                z ((c,t):ts) = do 
                        sleepThreadUntil t 
                        atomically $ do 
                                b <- readTVar repeat 
                                when (c `mod` b == 0) . join $ readTChan modi `orElse` return (return ())
                                writeTChan sync (c, t)
                        z ts
        w <- forkIO $ z times

        com <- newTVarIO Nothing 
        tps <- newTVarIO empty :: IO (TVar (Map String Double))
        tpp <- newTVarIO empty 
        ths <- newTVarIO []
        u <- forkIO $  track scNoteOn  tps tpp com sync ths 

        let     loop mcd = do 
                        o <- getInputLineWithInitial ":> " $ fromMaybe ("","") mcd
                        case o of 
                                Nothing -> return ()
                                Just o -> case o of
                                        "" -> liftIO (resolve S) >> loop Nothing 
                                        o -> case reads o of 
                                                [(c,_)] -> liftIO (resolve c) >>= loop
                                                _ -> outputStrLn "garbled or unknown command" >> loop mcd
        
                resolve (Sa f) =  do    forkIO $ do
                                                pp <- atomically $ readTVar tpp
                                                t <- atomically $ readTVar ths    
                                                l <- atomically $ readTVar com    
                                                writeFile f $ show (pp,t,l)
                                        return Nothing
                resolve Rs = bootSamples >> bootSynths >> return Nothing
                resolve (Ld f) =  do
                                        (pp,t,l) <- read `fmap` readFile f 
                                        void . forkIO . deepseq l . atomically .  writeTChan modi $ do 
                                                writeTVar ths t
                                                writeTVar com l
                                                writeTVar tpp pp        
                                        return Nothing

                resolve (Uc i j) = do   atomically . writeTChan modi . modifyTVar ths $ \xs -> 
                                                                        map snd . select (zip [0..] xs) i $ \(Track _ mph ph l a) -> Track j mph ph l a
                                        return Nothing

                resolve (Ph i ph) = do  atomically . writeTChan modi . modifyTVar ths $ \xs -> 
                                                map snd . select (zip [0..] xs) i $ \(Track j mph _ l a) -> Track j mph ph l a
                                        return Nothing
                                                
                resolve (Mph i mph) = do        
                                        atomically . writeTChan modi . modifyTVar ths $ \xs ->
                                                map snd . select (zip [0..] xs) i $ \(Track j _ ph l a) -> Track j mph ph l a
                                        return Nothing

                resolve (Wd i l) =   do atomically . writeTChan modi $ modifyTVar ths $ \xs ->
                                                map snd . select (zip [0..] xs) i $ \(Track j mph ph _ a) -> Track j mph ph l a
                
                                        return Nothing
                resolve (Pr i v) =   do atomically . writeTChan modi $ modifyTVar ths $ \xs ->
                                                map snd . select (zip [0..] xs) i $ \(Track j mph ph l a) -> let 
                                                        a' = case a of 
                                                                Play dt na as _ -> Play dt na as v
                                                                x -> x 
                                                        in Track j mph ph l a' 
                                        return Nothing
                resolve (Ac i a) = do   atomically . writeTChan modi $ modifyTVar ths $ \xs ->
                                                map snd . select (zip [0..] xs) i $ \(Track j mph ph l _) -> Track j mph ph l a
                                        return Nothing 
                resolve (T i t) = do    atomically . writeTChan modi $ modifyTVar ths $ \xs ->
                                                map snd . select (zip [0..] xs) i $ const t
                                        return Nothing
                resolve (S) = do 
                        ts <- atomically $ readTVar ths
                        forM_ (zip [0..] ts) $ \(i,t) -> putStrLn (show i ++ ") " ++ show t)
                        return Nothing
                resolve (Sp k v) = do   atomically $ modifyTVar tps $ insert k v 
                                        return Nothing
                resolve (Sc x) = mapM_ resolve  x >> return Nothing
                                                    
                resolve (R t) =  do     atomically . writeTVar repeat $ t
                                        return Nothing
                resolve (F f) =  do 
                                        x <- readFile f
                                        let ls = lines x
                                        let g = read $ last ls
                                        forkIO . deepseq g . atomically $ writeTChan modi $ do 
                                                writeTVar com $ Just g
                                        return Nothing 
                resolve (M i) = do 
                                t <- (!! i) `fmap` atomically (readTVar ths)
                                return $ Just ("T (O " ++ show i ++ ") (",show t ++ ")")
                resolve (N (Just i))  = do 
                                t <- atomically (readTVar ths)
                                let n = length t
                                let tn = t !! i
                                atomically $ writeTVar ths (t ++ [tn])
                                resolve (M n)
                resolve (D i) = do 
                                t <- zip [0..] `fmap` atomically (readTVar ths)
                                atomically $ writeTVar ths $ map snd $ deleteBy ((==) `on` fst) (i,undefined) t
                                return Nothing 
        runInputT (defaultSettings{ historyFile = Just "Main.hist", autoAddHistory = True}) $ loop Nothing
        killThread w
        killThread u
