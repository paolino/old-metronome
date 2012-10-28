

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
import Sound.OpenSoundControl (OSC (..), Time (..),utcr, sleepThreadUntil,sleepThread)
import System.Environment
import System.Exit
import System.Console.Haskeline
import Data.Lens.Lazy
import Control.Arrow (second)
import Data.Ratio
import Control.Monad.Random (evalRandIO)
import Control.DeepSeq
                        
import Schedule        
import Cursor
cs = withSC3 . flip send 


suona :: Synth
suona mt s  rs = do
        let a = s_new s (-1) AddToTail 1 $ rs
        cs . Bundle (UTCr (mt + 0.3)) $ [a]

        
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


{-
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
-}
main = do 
        r <- var empty :: IO (TVar State)
        f1 <- ((!! 100) . iterate right) `fmap` mkSF 16 30 [(1,Sh 0.2),(1,Sh 0.4),(1,Re 0.4),(1,Re 0.5)]
        f3 <- ((!! 100) . iterate right) `fmap` mkSF 16 30 [(1,Sh 0.2),(1,Sh 0.1),(1,Re 0.8),(1,Re 0.5)]
        f2 <- ((!! 100) . iterate right) `fmap`  mkSF 16 30 [(1,Sh 0.1),(1,Sh 0.2),(1,Re 0.8),(1,Re 0.9)]
        f4 <- ((!! 100) . iterate right) `fmap`  mkSF 16 30 [(1,Sh 0.1),(1,Sh 0.2),(1,Re 0.8),(1,Re 0.9)]
        t1 <- var $ Track 0 0.125 (Play "kick" 0 [("ampk","amp")]) f1
        t3 <- var $ Track 0.125 0.125 (Play "zill" 0 [("ampz","amp"),("ratez","rate")]) f3
        q1 <- forkTrack suona r t1
        q3 <- forkTrack suona r t3

        t2 <- var $ Track (-0.01) 0.125 (Parameter 0.6 0.0001 "ampk") f2
        t4 <- var $ Track (-0.01) 0.125 (Parameter 0.6 0.01 "ampz") f3
        t5 <- var $ Track (-0.01) 0.125 (Parameter 8.6 0.3 "ratez") f1
        q2 <- forkTrack suona r t2
        q4 <- forkTrack suona r t4
        q5 <- forkTrack suona r t5
        s1 <- forkIO $ forever $ atomically (modifyTVar t1 $ pattern ^%= right) >> sleepThread 1
        s2 <- forkIO $ forever $ atomically (modifyTVar t2 $ pattern ^%= right) >> sleepThread 0.5
        s3 <- forkIO $ forever $ atomically (modifyTVar t3 $ pattern ^%= right) >> sleepThread 0.5
        s4 <- forkIO $ forever $ atomically (modifyTVar t4 $ pattern ^%= right) >> sleepThread 1.5
        s5 <- forkIO $ forever $ atomically (modifyTVar t5 $ pattern ^%= right) >> sleepThread 1.5
        

        getLine

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
