
{-# LANGUAGE DoRec, StandaloneDeriving,NoMonomorphismRestriction, UndecidableInstances, DeriveTraversable, DeriveFunctor, DeriveFoldable, FlexibleContexts, TypeFamilies ,  DataKinds, TypeFamilies, GADTs, KindSignatures, ScopedTypeVariables #-}


import Prelude hiding (lookup, Left, Right)
import Data.Map (Map, insert, fromList, assocs, update, adjust, lookup, empty, findWithDefault, singleton)
import Data.Monoid (Monoid (..))
import Data.Maybe (fromMaybe)
import Data.List (isPrefixOf, foldr1) 
import Control.Arrow (first)
import Control.Monad
import Control.Monad.Reader 
import Control.Concurrent.STMOrIO 
import Control.Concurrent
import Control.Concurrent.STM (atomically, newTVarIO, modifyTVar, TVar, STM, readTChan, readTVar, dupTChan, newBroadcastTChanIO, writeTChan, writeTVar)
import Sound.SC3 hiding (Binary,select,Linear)
import Sound.OpenSoundControl (OSC (..), Time (..),utcr, sleepThreadUntil )
import System.Environment
import System.Exit
import System.Console.Haskeline

import Control.Arrow (second)
import Data.Ratio
import Algorythm
import Sequences
import Control.Monad.Random (evalRandIO)
import Control.DeepSeq
-- udp sending. fmap (tails . drop n) .  iterateM (fractal y) 


-- qfi :: Integer -> Int -> Int -> Sq Double Linear -> Sq Double a -> Double -> Integer -> Integer -> IO () 
-- track t k ph d x n m y = (map (map $ quantize k) `fmap` fractalFromTo n m y (zip [0,1/fromIntegral d .. 1] $ repeat x)) >>= mapM_ (render suona  t . Q k ph . T)
-- track' t k ph d x n m y = fractalStat  
data Lang = Still | Left Int | Right Int | Volume Double | Quit | Phase Double deriving (Show,Read)
data State = State Lang Double Double 
type IdLang = (Int,Lang)
track ch (q,s,ph,w,v,r,y,n) = do
        l <- newTVarIO (State Still 1 0)
        let cv = do 
                State _ v ph <- atomically $ readTVar l
                return $ (ph,adjust (*v) "amp")
        wi <- evalRandIO $ fractalSlideStat (foldr collapseA []) w 15 y $ zip [0,1/n..1] $ repeat [((s,[("rate",r)]),[("amp",1/n/8)])] 
        let k x = do
                render (suona cv) (v*60/128/2) . Q w ph $ core x
                f <- atomically $ readTVar l
                case f of
                        State Still _ _-> k x
                        State (Left n) _ _ ->  k $ (iterate left x) !! n
                        State (Right n) _ _ -> k $ (iterate right x) !! n
                        State Quit _ _ -> return ()
        ch' <- atomically $ dupTChan ch
        let h = do
                        r <- atomically $ do
                                (q',lx) <- readTChan ch'
                                if (q == (q' :: Int)) then do 
                                        case lx of
                                                Phase ph -> modifyTVar l (\(State s v _) -> State s v ph) >> return True
                                                Volume v -> modifyTVar l (\(State s _ ph) -> State s v ph) >> return True
                                                Quit -> modifyTVar l (\(State _ v ph) -> State Quit v ph) >> return False
                                                s -> modifyTVar l (\(State _ v ph) -> State s v ph) >> return True

                                else return True
                                
                        when r h
        forkIO (k wi)
        forkIO h
        return ()
                                
                        
                

cs = withSC3 . flip send 

type PControl = IO (Double, Map String Double -> Map String Double)

suona :: PControl -> Render
suona cn syns mt = do
        (ph,ps) <- cn
        let each  ((name,fs), vs)  = s_new name (-1) AddToTail 1 $ assocs. ps . fromList $ collapse sum $ fs ++ vs
        cs . Bundle (UTCr (mt + 1.5 + ph)) $ map each syns 

        
-- p0 = PL "kick" Nothing

perc :: UGen
perc = control KR "amp" 0.7 * envGen AR 1 1 0 1 RemoveSynth (envPerc (control KR "attacco" 0) (control KR "discesa" 1))

pb :: UGen -> UGen
pb n = playBuf 2 AR (fromIntegral n) (control KR "rate" 1) 0 0 NoLoop RemoveSynth * perc

synths n = 
        [       (""     ,pb n)
        ,       (".bpf" ,bpf (pb n) (control KR "bottom" 20) (control KR "top" 200))
        ,       (".brf" ,brf (pb n) (control KR "bottom" 20) (control KR "top" 200))
        ]

samples = zip ["kick","ride","bass","violin","violin2","zill","rullante"] [1..]

extension = ".wav"
sampledir = "/home/paolino/Music/"

msin = sinOsc AR (control KR "b" 80 + 
                (1/control KR "w" 8 * control KR "b" 80 * sum 
                        (map (\n -> sinOsc KR (480 / control KR "z" 125 * control KR n 1) 0) ["f1","f2","f3"])
                )
        ) (control KR "p" 90)

bootSynths = do 
        cs $ g_new [(1, AddToTail, 0)] 
        -- load samples
        forM_ samples $ \(i,n) -> cs $ b_allocRead n (sampledir ++ i ++ extension) 0 0
        -- create synths
        forM_ samples $  \(i,n) 
                -> forM_ (synths n) $ \(e,s) 
                        -> cs . d_recv . synthdef (i ++ e) . out 0 $ s
        cs . d_recv . synthdef "wow" . out 0 . (\s -> pan2 s 0 1) $ perc * msin


data Control = A IdLang | N (Int,String,Integer,Integer,Double,Double,[(Double,Linear)],Double) deriving (Read)
main = do 
        ch <- newBroadcastTChanIO
        runInputT (Settings completeFilename (Just ".supercollider2.history") True) $ forever $ do
        mcom <- getInputLine "> "
        case mcom of 
                Nothing -> lift $ exitSuccess 
                Just e -> case reads e of 
                        [(N x,_)] -> lift $ void $ track ch x 
                        [(A x,_)] -> lift . atomically $ writeTChan ch x
                        _ -> outputStrLn "no parse!"

{-
main' = do
        (f:p:n:_) <- getArgs
        bootSynths
        tstv <- var True
        m <- mkMetronome (RealTime $ 60/read n/read p) 
        ms <- newTVarIO []
        sc <- forkIO $ run suona ms m 
        runInputT (Settings completeFilename (Just ".supercollider2.history") True) $ 
                let loop = do 
                        mcom <- getInputLine "command> " 
                        case mcom of 
                                Nothing -> lift $ killThread sc 
                                Just com -> const loop =<< case reads com of 
                                        [(x,_)] -> lift . atomically . modifyTVar ms . (:) $ x
                                        _ -> outputStrLn "no parse!"
                in loop
-}
