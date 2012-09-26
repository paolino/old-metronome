{-# LANGUAGE DoRec, StandaloneDeriving,NoMonomorphismRestriction, UndecidableInstances, DeriveTraversable, DeriveFunctor, DeriveFoldable, FlexibleContexts, TypeFamilies , DatatypeContexts, DataKinds, TypeFamilies, GADTs, KindSignatures #-}

module Supercollider where


import Control.Monad.Random hiding (fromList, MonadRandom)
import Control.Monad.State (evalState)
import Control.Applicative
import Data.Ratio 
import Data.List hiding (insert, concat,all, concatMap,sum,foldr1)
import Prelude hiding ((.),mapM, concat,all,concatMap,sum,mapM_,foldr1)
import Data.Map (Map, insert, fromList, assocs, update, adjust)
import Data.Monoid
import Data.Traversable
import Data.Foldable
import Control.Monad.Reader hiding (mapM, mapM_)
import Data.Ord (comparing)
import System.Metronome.Practical
import System.Metronome
import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent.STMOrIO
import Control.Concurrent.STM
import Control.Concurrent
import Sound.SC3 hiding (Binary,select) -- (withSC3, send, g_new, d_recv, synthdef, out , mce, control, Rate (..), sinOsc, envGen, DoneAction (..), envPerc, s_new, AddAction (AddToTail))
import Sound.SC3.UGen.Noise.ID 
import Sound.OpenSoundControl 
import Data.Lens.Lazy
import Control.Arrow 
import Data.Random
import Data.Random.Distribution.Categorical (categorical)
import Control.Category ((.))
import Monad
import Data.Ratio
import Rythmics
import Language
import Binary

-- udp sending
cs = withSC3 . flip send 

tcs :: OSC -> Action
tcs x = ask >>= \t -> lift . return . cs . Bundle (UTCr t) . return $ x

-- add group 1
addGroup1 = cs $ g_new [(1, AddToTail, 0)]

-- add a stereo synth
addSynth name = cs . d_recv . synthdef name . out 0

-- control parameter
par = control KR 

-- symbols
freq = "freq"
amp = "amp"
attacco = "attacco"
discesa = "discesa"
-- parametric sine
sine f = sinOsc AR (par f 220) 0 
perc = par amp 0.2 * envGen AR 1 1 0 1 RemoveSynth (envPerc (par attacco 1) (par discesa 2))

percV :: STMOrIO m => m Params
percV = var $ fromList [(attacco,0.05),(discesa,0.5),(amp,0.2)]

type MP = Map String Double
type Params = TVar MP

(*^) :: Int -> [a] -> [a]
n *^ xs = concat $ replicate n xs

inf = (*^) maxBound 

suona :: String -> Params -> Action
suona name fs = rd fs >>= tcs . s_new name (-1) AddToTail 1 . assocs


bootBinary pace subd = do
        let ticks = 2 ^ subd
        (cm,kt) <- mkMetronome $ pace/ fromIntegral ticks 
        return (cm,killThread kt)


bootSynths = do 
        addGroup1
        cs $ b_allocRead 1 "/home/paolino/Music/bass.wav" 0 0
        let s r =  klankSpec ([80, r 120, r 150]) [1,1,1,1] [1,1,1,1]
        addSynth "klank" $ (dynKlank 1 1 0 1 (s (* control KR "rate" 1)) * perc)
        addSynth "noise" $ whiteNoise 'a' AR * 0.1 * perc 
        addSynth "s" $ sine "f1" * sine "f2" * perc
        Data.Foldable.forM_ (zip [1..] ["kick","ride","bass"]) $ \(n,i) -> do 
                cs $ b_allocRead n ("/home/paolino/Music/" ++ i ++ ".wav") 0 0
                addSynth i $ playBuf 2 AR (fromIntegral n) (control KR "rate" 1) 0 0 NoLoop RemoveSynth * perc



file f m = 
        load `fmap` (Prelude.map read `fmap` lines` fmap` readFile f :: IO (Piece Binary)) 
        >>=  uncurry (boot suona m)
