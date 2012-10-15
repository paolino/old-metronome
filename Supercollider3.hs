
{-# LANGUAGE DoRec, StandaloneDeriving,NoMonomorphismRestriction, UndecidableInstances, DeriveTraversable, DeriveFunctor, DeriveFoldable, FlexibleContexts, TypeFamilies ,  DataKinds, TypeFamilies, GADTs, KindSignatures, ScopedTypeVariables #-}


import Prelude hiding (lookup)
import Data.Map (Map, insert, fromList, assocs, update, adjust, lookup, empty, findWithDefault, singleton)
import Data.Monoid (Monoid (..))
import Data.Maybe (fromMaybe)
import Data.List (isPrefixOf, foldr1) 
import Control.Arrow (first)
import Control.Monad
import Control.Monad.Reader 
import Control.Concurrent.STMOrIO 
import Control.Concurrent
import Control.Concurrent.STM (atomically, newTVarIO, modifyTVar, TVar, STM)
import Sound.SC3 hiding (Binary,select,Linear)
import Sound.OpenSoundControl (OSC (..), Time (..),utcr, sleepThreadUntil )
import System.Environment
import System.Console.Haskeline

import Control.Arrow (second)
import Data.Ratio
import Algorythm
import Sequences
-- udp sending

cs = withSC3 . flip send 
suona :: Render
suona syns mt = do
        let each  ((name,fs), vs)  = s_new name (-1) AddToTail 1 $ collapse sum $ fs ++ vs
        t0 <- utcr
        cs . Bundle (UTCr (mt + 5)) . map each $ syns

        
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


fac n m = take (m + 1) $ iterate (*n) 1 
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
