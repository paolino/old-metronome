

{-# LANGUAGE DoRec, StandaloneDeriving,NoMonomorphismRestriction, UndecidableInstances, DeriveTraversable, DeriveFunctor, DeriveFoldable, FlexibleContexts, TypeFamilies ,  DataKinds, TypeFamilies, GADTs, KindSignatures, ScopedTypeVariables #-}

module Supercollider4 where

import Prelude hiding (lookup, Left, Right)
import Control.Monad
import Sound.SC3 hiding (Binary,select,Linear)
import Sound.SC3.UGen.Noise.ID 
import Sound.OpenSoundControl (OSC (..), Time (..),utcr, sleepThreadUntil,sleepThread)
import Data.Map (assocs)
                        
import Actions

cs = withSC3 . flip send 

scNoteOn :: NoteOn
scNoteOn mt s  rs = do
        let     a = s_new s (-1) AddToTail 1 $ assocs rs
        t0 <- last (show a) `seq` utcr
        if t0 < (mt + 3)  then  cs . Bundle (UTCr (mt + 3)) $ [a]
                else print "late!"
        
-- p0 = PL "kick" Nothing

perc :: UGen -> UGen
perc k = control KR "amp" 0.8 * envGen AR 1 1 0 1 RemoveSynth (envPerc (control KR "attacco" 0) (control KR "discesa" 3 * 2/ k))

keyb :: UGen
keyb = control KR "amp" 0.5 * envGen AR 1 1 0 1 RemoveSynth (envTrapezoid 0 (control KR "attacco" 0 / 3) (control KR "discesa" 3 ) 1)

tick = envGen AR 1 1 0 1 DoNothing (envPerc 0 0.2)
pb :: UGen -> UGen
pb n = playBuf 2 AR (fromIntegral n) (1.2 + control KR "rate" 1/100) 0 0 NoLoop RemoveSynth * perc 1

synths n = 
        [            
                (""     ,(pb n))
        --        ,       (".bpf" ,bpf (pb n) (control KR "bottom" 20) (control KR "top" 200))
--        ,       (".brf" ,brf (pb n) (control KR "bottom" 20) (control KR "top" 200))
        ]

samples = zip ["kick","ride","zill","rullante", "violin","bass"] [1..]

extension = ".wav"
sampledir = "/home/paolino/Music/"

msin = sinOsc AR (control KR "b" 80 + 
                (1/control KR "w" 8 * control KR "b" 80 * sum 
                        (map (\n -> sinOsc KR (480 / control KR "z" 125 * control KR n 1) 0) ["f1","f2","f3"])
                )
        ) (control KR "p" 90)

percz = control KR "amp" 0.01 * envGen AR 1 1 0 1 RemoveSynth (envPerc (control KR "attacco" 0.2) (control KR "discesa" 1/2))

snare q k l =  out 0 . (\s -> pan2 s (0.05 * sinOsc KR 6 0) 1) $ q * sum ([n * z k | (n,k) <- [(0.8,1),(0.2,2),(0.2,2.01)]])where
        z f = perc 1 * lpf (0.1 * ringz  (sinOsc AR 1 0.4) (l * f * freq) 30) (2*l * f * freq)
        freq = midiCPS $ control KR "freq" 45 +  control KR "rate" 0

zill = out 0 . (\s -> pan2 s (0.05 * sinOsc KR 6 0) 1) $  percz * 0.00 * ringz (pinkNoise 'a' AR) (freq * 6)  2
        where   freq = midiCPS $ control KR "freq" 45  
sino = out 0  .(\s -> pan2 s (0.05 * sinOsc KR 6 0) 1) $ perc 2 * (0.6 + (0.05 * sinOsc KR 5 0)) * ( sinOsc AR (3 * freq) 0 * sinOsc AR (freq * 2) 0) *
        ( 0.7 * sinOsc AR freq 0
        + 0.02 * sinOsc AR (freqn 5 ) 0
        + 0.02 * sinOsc AR (freqn 7 ) 0
        + 0.02 * sinOsc AR (freqn 10) 0
        )
        where   freq = midiCPS $ control KR "freq" 45 + control KR "rate" 0
                freqn n = midiCPS $ control KR "freq" 45 + control KR "rate" 0 + n
freq = 110


bootSynths = do 
        cs $ p_new [(1, AddToTail, 0)] 
        cs . d_recv . synthdef "sino" $ sino 
        cs . d_recv . synthdef "zill" $ zill
        cs . d_recv . synthdef "rullante" $ snare 0.1 1 2
        cs . d_recv . synthdef "kick" $ snare 0.1 0.5 1
--        cs . d_recv . synthdef "wow" . out ((n - 1) *2) . (\s -> pan2 s 0 1) $ perc * msin

bootSamples = do
        cs $ p_new [(1, AddToTail, 0)] 
        -- load samples
        forM_ samples $ \(i,n) -> cs $ b_allocRead n (sampledir ++ i ++ extension) 0 0
        -- create synths
        forM_ samples $  \(i,n) 
                -> forM_ (zip [0..] $ synths n) $ \(j,(e,s))  
                        -> cs . d_recv . synthdef (i ++ e) . out 0 $ s
