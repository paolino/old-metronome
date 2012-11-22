{-# LANGUAGE DoRec, StandaloneDeriving,NoMonomorphismRestriction, UndecidableInstances, DeriveTraversable, DeriveFunctor, DeriveFoldable, FlexibleContexts, TypeFamilies ,  DataKinds, TypeFamilies, GADTs, KindSignatures, ScopedTypeVariables #-}

module Supercollider4 where

import Prelude 
import Control.Monad
import Sound.SC3 hiding (Binary,select,Linear)
import Sound.SC3.UGen.Noise.ID 
import Sound.SC3.UGen.FFT 
import Sound.OpenSoundControl (OSC (..), Time (..),utcr, sleepThreadUntil,sleepThread)
import Data.Map (assocs)
import Data.List (find)
                        
import Actions


freq :: UGen
freq  = midiCPS $ control KR "rate" 0

cs = withSC3 . flip send 

scNoteOn :: NoteOn
scNoteOn mt (Left s)  rs = do
        let     a = s_new s (-1) AddToTail 1 $ assocs rs
        t0 <- last (show a) `seq` utcr
        if t0 < (mt)  then  cs . Bundle (UTCr (mt)) $ [a]
                else print "late!"
scNoteOn mt (Right s)  rs = do
        let     a = n_set s $ assocs rs
        t0 <- last (show a) `seq` utcr
        if t0 < (mt)  then  cs . Bundle (UTCr (mt)) $ [a]
                else print "late!"
        
baz = out 0 . (\s -> pan2 s 0 1) $ control KR "amp" 0.2 * ( 
                mix $ hpf (lpf (0.5 * (lfSaw AR (fs 1) 0)) (freq * 2)) (freq * 0.8))
                        
                        where
        fs l = mce [freq,freq * 1.007]

bazzo = out 0 . (\s -> pan2 s 0 1) $ perc 1 * ( 
                mix $ hpf (lpf (0.2 * (0.6*lfSaw AR (fs 1) 0)) (freq * 4)) (freq * 0.6))
                        
                        where
        fs l = mce [freq,freq * 1.007,freq * 1.003]

-- p0 = PL "kick" Nothing

perc :: UGen -> UGen
perc k = control KR "amp" 0.8 * envGen AR 1 1 0 1 RemoveSynth (envPerc (control KR "attacco" 0) (control KR "discesa" 1 * 2/ k))

keyb :: UGen
keyb = control KR "amp" 0.5 * envGen AR 1 1 0 1 RemoveSynth (envTrapezoid 0 (control KR "attacco" 0 / 3) (control KR "discesa" 3 ) 1)

tick = envGen AR 1 1 0 1 DoNothing (envPerc 0 0.2)
pb :: UGen -> UGen
pb n = playBuf 2 AR (fromIntegral n) ((1 + control KR "shift" 0) * (control KR "rate" 0 + 1)/midiCPS 0) 0 0 NoLoop RemoveSynth * perc 1

synths n = 
        [            
                (""     ,(pb n))
        --        ,       (".bpf" ,bpf (pb n) (control KR "bottom" 20) (control KR "top" 200))
--        ,       (".brf" ,brf (pb n) (control KR "bottom" 20) (control KR "top" 200))
        ]

samples = zip ["kick","ride","zill","rullante"] [1..]

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


bootSynths = do 
        cs . d_recv . synthdef "baz" $ baz 
        cs . d_recv . synthdef "bazzo" $ bazzo
        sleepThread 1
        cs $ n_free [3]
        cs $ s_new "baz" 3 AddToTail 1 []
--        cs $ s_new "baz" 4 AddToTail 1 []
--        cs $ s_new "baz" 5 AddToTail 1 []
        
        -- cs $ s_new "sin" 2 AddToTail 1 $ []
         
--        cs . d_recv . synthdef "wow" . out ((n - 1) *2) . (\s -> pan2 s 0 1) $ perc * msin

bootSamples = do
        cs $ p_new [(1, AddToTail, 0)] 
        -- load samples
        forM_ samples $ \(i,n) -> cs $ b_allocRead n (sampledir ++ i ++ extension) 0 0
        -- create synths
        forM_ samples $  \(i,n) 
                -> forM_ (zip [0..] $ synths n) $ \(j,(e,s))  
                        -> cs . d_recv . synthdef (i ++ e) . out 0 $ s

boot = do
        cs $ p_new [(1, AddToTail, 0)] 
        sleepThread 1
        bootSynths
        bootSamples

