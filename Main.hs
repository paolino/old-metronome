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
import Data.Map (empty, fromList,Map,insert,assocs)
import Control.Monad.Random hiding (fromList)
import Data.Maybe (catMaybes)
import Control.DeepSeq
import Control.Monad.Trans
import Control.Arrow
import Data.Array hiding (assocs)
import Data.Maybe (fromMaybe)
import Data.Function (on)

import Data.List hiding (insert)
import Data.Ord (comparing)
import Debug.Trace
import Control
type Index = Int

data Track = Track (Control String) (Control String) (Control String) (Control String) Action deriving (Show,Read,Eq)

instance Ord Track where
        compare (Track _ _ _ _ a) (Track _ _ _ _ a') = compare a a'

fromControlInt mp mv x y = floor $ fromControl mp mv x y

track :: NoteOn -> TVar (Params Double)  -> TVar (Map String [(Double,Double)]) -> TVar (Maybe (Array (Int,Int) Double)) -> TChan (Int,Tempo) -> TVar [Track] -> IO ()
track noteon tmv tmp tls tc tv = track' where
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
                                mv <- atomically $ readTVar tmv
                                mp <- atomically $ readTVar tmp        
                                let  z (Track i' mph' ph' l' x)  = let 
                                        [mph,i,ph,l] = map (fromControlInt mp mv 0) [mph',i',ph',l']
                                        in render noteon tmv tmp x (c l mph ph) (v i mph ph l) (vs i) t
                                actCollect $ map z rs
                track'  


data Reg = O Int | Lo [Int] | Al deriving (Show,Read)

data Rep = On Int | Mu [Int]

data Command = Sm | St | F String | Ph Reg (Control String) | Wd Reg (Control String) | Sa (Maybe String) | Ld String | Mph Reg (Control String) | Sc [Command] | R Int 
                | Uc Reg (Control String) | Pr Reg (Control String) | Rs | Sp String Double | Ac Reg Action | Tr Reg Track | Mo Int | New Int| D Int | Sh Int deriving (Show,Read)

select :: [(Int,a)] -> Reg -> (a -> a) -> [(Int,a)]
select xs Al f = map (second f) xs
select xs (O x) f =  select xs (Lo [x]) f
select [] (Lo ms) f = []
select ((i,x):xs) (Lo ms) f = (i,if i `elem` ms then f x else x):select xs (Lo ms) f
        
dolcezza :: Num a => [a] -> a
dolcezza xs = sum . zipWith (\x y -> abs (x - y)) xs $ tail $ cycle xs

smoother lcom = let       ((0,0),(mi,mj)) = second ((+1) *** (+1)) $ bounds lcom
                          vs i = [lcom ! (i `mod` mi ,j) | j <- [0..mj - 1]] 
                  in fst . minimumBy (comparing (dolcezza . snd)) $ zip [0..] $ map vs [0..mi -1]
d = 60/4/120
main = do
        boot
        sync <- newTChanIO :: IO (TChan (Int,Tempo))
        modi <- newTChanIO 
        repeat <- newTVarIO 1
        t0' <- utcr

        com <- newTVarIO Nothing 
        tps <- newTVarIO empty :: IO (TVar (Map String Double))
        tpp <- newTVarIO empty 
        ths <- newTVarIO []
        save <- newTVarIO "undefined.rec"

        u <- forkIO $  track scNoteOn  tps tpp com sync ths 

        let     loop mcd = do 
                        o <- getInputLineWithInitial ":> " $ fromMaybe ("","") mcd
                        case o of 
                                Nothing -> return ()
                                Just o -> case o of
                                        "" -> liftIO (resolve St) >> loop Nothing 
                                        o -> case reads o of 
                                                [(c,_)] -> liftIO (resolve c) >>= loop
                                                _ -> outputStrLn "garbled or unknown command" >> loop mcd
        
                resolve (Sa mf) =  do   forkIO $ do
                                                pp <- atomically $ readTVar tpp
                                                t <- atomically $ readTVar ths    
                                                l <- atomically $ readTVar com    
                                                f <- case mf of 
                                                        Nothing -> atomically (readTVar save) 
                                                        Just f -> atomically (writeTVar save f) >> return f
                                                writeFile f $ show (pp,t,l)
                                        return Nothing
                resolve Rs = bootSamples >> bootSynths >> return Nothing
                resolve (Ld f) =  do
                                        (pp,t,l) <- read `fmap` readFile f 
                                        void . forkIO . deepseq l . atomically .  writeTChan modi $ do 
                                                writeTVar ths t
                                                writeTVar com l
                                                writeTVar tpp pp        
                                                writeTVar save f
                                        
                                        return Nothing

                resolve (Uc i j) = do   atomically . writeTChan modi . modifyTVar ths $ \xs -> 
                                                                        map snd . select (zip [0..] xs) i $ \(Track _ mph ph l a) -> Track j mph ph l a
                                        return Nothing
                resolve Sm = do lcom <- atomically $ readTVar com
                                let j = N $ maybe 0 smoother lcom
                                atomically . writeTChan modi . modifyTVar ths $ \xs -> 
                                        map snd . select (zip [0..] xs) Al $ \(Track _ mph ph l a) -> Track j mph ph l a
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
                resolve (Tr i t) = do   atomically . writeTChan modi $ modifyTVar ths $ \xs ->
                                                map snd . select (zip [0..] xs) i $ const t
                                        return Nothing
                resolve (St) = do 
                        ts <- atomically $ readTVar ths
                        tp <- atomically $ readTVar tps
                        forM_ (sortBy (comparing snd) $ zip [0..] ts) $ \(i,t) -> putStrLn (show i ++ ") " ++ show t)
                        forM_ (assocs tp) $ \(n,v) -> putStrLn (n ++ " -> " ++ show v)
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
                resolve (Mo i) = do 
                                t <- (!! i) `fmap` atomically (readTVar ths)
                                return $ Just ("Tr (O " ++ show i ++ ") (",show t ++ ")")
                resolve (New i)  = do 
                                t <- atomically (readTVar ths)
                                let n = length t
                                let tn = if i < n then t !! i else Track (N 0) (N 0) (N 0) (N 0) (SParam "" [])
                                atomically $ writeTVar ths (t ++ [tn])
                                resolve (Mo n)
                resolve (D i) = do 
                                t <- zip [0..] `fmap` atomically (readTVar ths)
                                atomically $ writeTVar ths $ map snd $ deleteBy ((==) `on` fst) (i,undefined) t
                                return Nothing 
                resolve (Sh d) = do 
                                g <- newStdGen
                                let rs = map (N . fromIntegral . (`mod` 256) . (*d)) $ randoms g 
                                atomically . writeTChan modi $ modifyTVar ths $ zipWith (\r (Track j mph _ l a) -> Track j mph r l a) rs 
                                return Nothing 
                                
        let     m =    floor $  t0' / d
                t0 = fromIntegral (m + 1) * d
                times = map (id &&& ((t0 +).(*d). fromIntegral)) [0..]
                z ((c,t):ts) = do 
                        sleepThreadUntil t 
                        sa <- atomically $ do 
                                b <- readTVar repeat 
                                writeTChan sync (c, t)
                                if c `mod` b == 0 then join $ (fmap (>> return True) $ readTChan modi) `orElse` return (return False)
                                        else return False
                        when sa $ void $ resolve (Sa Nothing)
                        z ts
        w <- forkIO $ z times
        runInputT (defaultSettings{ historyFile = Just "Main.hist", autoAddHistory = True}) $ loop Nothing
        killThread w
        removeSynth
        killThread u
