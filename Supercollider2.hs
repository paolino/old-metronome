{-# LANGUAGE DoRec, StandaloneDeriving,NoMonomorphismRestriction, UndecidableInstances, DeriveTraversable, DeriveFunctor, DeriveFoldable, FlexibleContexts, TypeFamilies , DatatypeContexts, DataKinds, TypeFamilies, GADTs, KindSignatures, ScopedTypeVariables #-}


import Prelude hiding (lookup)
import Data.Map (Map, insert, fromList, assocs, update, adjust, lookup, empty, findWithDefault)
import Data.List (isPrefixOf) 
import Control.Arrow (first)
import Control.Monad
import Control.Monad.Reader 
import Control.Concurrent.STMOrIO 
import Control.Concurrent
import Control.Concurrent.STM (atomically, newTVarIO, modifyTVar)
import Sound.SC3 hiding (Binary,select)
import Sound.OpenSoundControl (OSC (..), Time (..) )
import System.Environment
import System.Console.Haskeline

import Control.Arrow (second)
import Data.Ratio

import System.Metronome2
-- udp sending

cs = withSC3 . flip send 

data Suono = Su String deriving (Read,Show)

suona :: RealTime -> Shared String -> [(Suono, ParamName String)] -> IO ()
suona (RealTime t) sh xs = let
        each  (Su name, par)  = s_new name (-1) AddToTail 1 . map (first $ (\(K x) -> x)) .  assocs $ findWithDefault empty par sh  
        in      do      cs . Bundle (UTCr (t + 0.2)) . map each $ xs


perc :: UGen
perc = control KR "amp" 0.2 * envGen AR 1 1 0 1 RemoveSynth (envPerc (control KR "attacco" 0) (control KR "discesa" 1))

pb :: UGen -> UGen
pb n = playBuf 2 AR (fromIntegral n) (control KR "rate" 1) 0 0 NoLoop RemoveSynth * perc

synths n = 
        [       (""     ,pb n)
        ,       (".bpf" ,bpf (pb n) (control KR "bottom" 20) (control KR "top" 200))
        ,       (".brf" ,brf (pb n) (control KR "bottom" 20) (control KR "top" 200))
        ]

samples = zip ["kick","ride","bass","violin","violin2","zill"] [1..]

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

traks = fromList 
        [       ("kick", Track (Beat 4) 0 0 128 Infinite (Act (Su "kick") (PN "pk"))) 
        
        ]
 
--        [       TrackEnv "kick" (TrackCtx 0 0 128) False (RTrack (Action $ Suono "kick" "pk") [0,2%8,6%8])
--       ] 
       


main = do
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
 
