{-# LANGUAGE DoRec, StandaloneDeriving,NoMonomorphismRestriction, UndecidableInstances, DeriveTraversable, DeriveFunctor, DeriveFoldable, FlexibleContexts, TypeFamilies , DatatypeContexts, DataKinds, TypeFamilies, GADTs, KindSignatures, ScopedTypeVariables #-}


import Prelude hiding (lookup)
import Data.Map (Map, insert, fromList, assocs, update, adjust, lookup, empty)
import Data.List (isPrefixOf) 
import Control.Monad
import Control.Monad.Reader 
import System.Metronome.Practical
import System.Metronome
import Control.Concurrent.STMOrIO 
import Control.Concurrent
import Control.Concurrent.STM (atomically)
import Sound.SC3 hiding (Binary,select)
import Sound.OpenSoundControl (OSC (..), Time (..) )
import System.Environment

import Language (Piece)
import Parser (render, PMap, load)
import Control.Arrow (second)
import Data.Ratio
import Binary (Binary)
-- udp sending
cs = withSC3 . flip send 


suona :: String -> Control PMap -> Action
suona name fs = do 
                ps <- rd fs 
                case lookup "amp" ps of 
                        Nothing -> return (return ())
                        Just 0 -> return (return ())
                        _ -> tcs . s_new name (-1) AddToTail 1 . map (second realToFrac) . assocs $ ps
        where
        tcs :: OSC -> Action
        tcs x = ask >>= \t -> lift . return . cs . Bundle (UTCr t) . return $ x



bootMetronome pace subd = do
        let ticks = 2 ^ subd
        (cm,kt) <- mkMetronome $ pace/ fromIntegral ticks 
        return (cm,killThread kt)

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


data Line b = Line b | Comment (String,Int) | Error Int

partitionLine = foldr part ([],[],[]) where
        part (Line x) (xs,ys,zs) = (x:xs,ys,zs)
        part (Comment y) (xs,ys,zs) = (xs,y:ys,zs)
        part (Error z) (xs,ys,zs) = (xs,ys,z:zs)

readLine (n,l) 
        | "#" `isPrefixOf` l = Comment (tail l, n)
        | otherwise = case reads l of
                [(x,_)] -> Line x
                _ -> Error n

file f m lps =     do 
                vs <- zip [1..] `fmap` lines `fmap` readFile f 
                let     (ls,_,es) = partitionLine . map readLine $ vs
                        (ps,ts) = load (ls :: Piece Binary)
                case es of
                        [] -> atomically $ render suona m ps ts lps
                        es -> mapM_ (\n -> putStrLn ("can't read line: " ++ show n)) es >> return lps


main = do
        (p:x:n:_) <- getArgs
        (m,km) <- bootMetronome (480/read x) (read n)
        bootSynths
        let loop lps = do
                command <- getLine
                case command of
                                "l" -> file p m lps >>= loop
                                "s" -> km
                                "i" -> info m >>= mapM_ print >> loop lps
                                "t" -> bootSynths >> loop lps
                                _ -> loop lps
        loop empty
 
