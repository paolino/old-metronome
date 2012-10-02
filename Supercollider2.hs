{-# LANGUAGE DoRec, StandaloneDeriving,NoMonomorphismRestriction, UndecidableInstances, DeriveTraversable, DeriveFunctor, DeriveFoldable, FlexibleContexts, TypeFamilies , DatatypeContexts, DataKinds, TypeFamilies, GADTs, KindSignatures, ScopedTypeVariables #-}


import Prelude hiding (lookup)
import Data.Map (Map, insert, fromList, assocs, update, adjust, lookup, empty, findWithDefault)
import Data.List (isPrefixOf) 
import Control.Monad
import Control.Monad.Reader 
import Control.Concurrent.STMOrIO 
import Control.Concurrent
import Control.Concurrent.STM (atomically)
import Sound.SC3 hiding (Binary,select)
import Sound.OpenSoundControl (OSC (..), Time (..) )
import System.Environment

import Control.Arrow (second)
import Data.Ratio

import System.Metronome2
-- udp sending

cs = withSC3 . flip send 

data Suono = Suono String String

suona :: RealTime -> Shared String -> [Suono] -> IO ()
suona (RealTime t) sh xs = let
        each  (Suono name par)  = s_new name (-1) AddToTail 1 . assocs $ findWithDefault empty par sh
        in cs . Bundle (UTCr (t + 1)) . map each $ xs


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

traks = 
        [       TrackEnv "kick" (TrackCtx 0 0 128) False (RTrack (Action $ Suono "kick" "pk") [0,1%4,2%4,5%8,7%8])
        ,       TrackEnv "kickA" (TrackCtx 0 0 256) False (RTrack (IS ("pk","amp") 0.8) [0,1%4,2%4,7%8])
        ,       TrackEnv "kickAl" (TrackCtx 0 0 256) False (RTrack (IS ("pk","amp") 0.5) [0,1%3,2%3])
        ,       TrackEnv "zill" (TrackCtx 0 0 128) False (RTrack (Action $ Suono "zill" "pz") $ map (*(1%16)) [0..])
        ,       TrackEnv "kickUnphase" (TrackCtx 0 0 256) False (RTrack (MPh "kick" (S 8)) [0,1%3,2%3])
        ,       TrackEnv "kickRephase" (TrackCtx 0 0 512) False (RTrack (MPh "kick" (S (-16))) [0,1%6,5%7])
        ,       TrackEnv "zillUnphase" (TrackCtx 0 0 128) False (RTrack (MPh "zill" (S 4)) [0,1%3,2%3])
        ,       TrackEnv "zillRephase" (TrackCtx 0 0 128) False (RTrack (MPh "zill" (S (-4))) [0,1%6,5%7])
         
        ] 
        

main = do
        (p:n:_) <- getArgs
        bootSynths
        m <- mkMetronome mkTrack (RealTime $ 60/read n/read p) traks
        run suona m 
 
