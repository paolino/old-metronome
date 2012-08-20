{-# LANGUAGE DoRec, NoMonomorphismRestriction, DeriveTraversable, DeriveFunctor, DeriveFoldable, FlexibleContexts #-}

module Supercollider where


import Control.Monad.Random hiding (fromList)

import Data.Ratio 
import Data.List
import Prelude hiding ((.))
import Data.Map (Map, insert, fromList, assocs, update, adjust)
import Data.Monoid
import Control.Monad.Reader

import System.Metronome.Practical
import System.Metronome

import System.Metronome (Action, MTime)
import Control.Concurrent.STMOrIO
import Control.Concurrent.STM
import Sound.SC3.ID (withSC3, send, g_new, d_recv, synthdef, out , mce, control, Rate (..), sinOsc, envGen, DoneAction (..), envPerc, s_new, AddAction (AddToTail))
import Sound.OpenSoundControl 
import Data.Lens.Lazy
import Control.Category
import Control.Arrow (second)

-- udp sending
cs = withSC3 . flip send 

tcs :: OSC -> Action
tcs x = ask >>= \t -> lift . return . cs . Bundle (UTCr t) . return $ x

-- add group 1
addGroup1 = cs $ g_new [(1, AddToTail, 0)]

-- add a stereo synth
addSynth name = cs . d_recv . synthdef name . out (mce [0,1])

-- control parameter
par = control KR 

-- symbols
freq = "freq"
amp = "amp"
attack = "attack"
decay = "decay"

-- parametric sine
sine f = sinOsc AR (par f 220) 0 
perc = par amp 0.2 * envGen AR 1 1 0 1 RemoveSynth (envPerc (par attack 0.01) (par decay 0.5))

percV :: STMOrIO m => m Params
percV = var $ fromList [(attack,0.01),(decay,0.5),(amp,0.2)]

type MP = Map String Double
type Params = TVar MP

infixl 9 # 
(#) = (,)

infixl 8 *>
(*>) :: STMOrIO m => (MP -> MP) -> Params -> m ()
(*>)= flip md 

(*^) :: Int -> [a] -> [a]
n *^ xs = concat $ replicate n xs

inf = (*^) maxBound 

-- a player
play :: String -> Params -> Action
play name fs = rd fs >>= tcs . s_new name (-1) AddToTail 1 . assocs
                        




data Ritmo = Play | Pause | Split Int [Ritmo] deriving Show

eval :: Rational -> Ritmo -> [Either Rational Rational]
eval d Play = [Right d]
eval d Pause = [Left d]
eval d (Split n xs) = xs >>= eval (d/fromIntegral n)


scatter :: (Functor m, MonadRandom m) => Double -> Int -> m [Double]
scatter l n = do
        xs <- (sort . take (n - 1))  `fmap` getRandomRs (0,l)
        return $ zipWith (flip (-)) (0:xs) $ xs ++ [l]

evalScatter :: (Functor m, MonadRandom m) => Rational -> Double -> Ritmo -> m [Either Rational (Rational,Double)]
evalScatter d p Play = return [Right (d,p)]
evalScatter d p Pause = return [Left d]
evalScatter d p (Split n xs) = do
        ps <- scatter p n 
        fmap concat . mapM (uncurry $ evalScatter (d/fromIntegral n)) $ zip ps xs


	
pp True = Play
pp False = Pause

randomSplit :: (MonadRandom m, Functor m) => Int -> Ritmo -> m Ritmo
randomSplit _ Pause = fmap pp $ getRandom
randomSplit n Play = fmap (Split n) . fmap (map pp) $ replicateM n getRandom
randomSplit n (Split m xs) = do 
        k <- getRandomR (0,m - 1)
        let (bs,x:cs) = splitAt k xs
        (Split m . (bs ++) . (++ cs) . return) `fmap` randomSplit n x 
                 
randomRitmo2 :: (Functor m, MonadRandom m) => Int -> Double -> m [Either Duration (Duration, Double)]

randomRitmo2 n p = foldM (\x _ -> randomSplit 2 x) Play [1..n] >>= evalScatter 1 p

assignP :: (Double -> Action) -> [Either Duration (Duration, Double)] -> [(Duration,Action)]
assignP f = map . either (\t -> (t,return $ return ())) . second $ f

randomAction :: (Functor m, MonadRandom m) => Int -> Double -> (Double -> Action) -> m [(Duration , Action)]
randomAction n p fp = assignP fp `fmap` randomRitmo2 n p

	

-- > {-# LANGUAGE DoRec #-}
-- > 
-- > import System.IO
-- > import System.Metronome.Practical
-- > import Control.Concurrent.STMOrIO
-- > import Control.Monad
-- > ghc force garbage collection
-- > main = do
-- >       hSetBuffering stdout NoBuffering
-- >       (m,f) <- dummyMetronome 0.1
-- >       c <- dummyTrack f 2 0 $ replicate 5 $ return $ putStr "."
-- >       v <- var "!"  
-- >       c2 <- dummyTrack f 1 0 . repeat . noIO $ do
-- >                 as <- getActions c
-- >                 vl <- rd v
-- >                 when (null as) . setActions c . replicate 5 . return $ putStr vl
-- >       c3 <- dummyTrack f 14 0 . repeat . noIO . md v $ map succ
-- >       end <- chan ()
-- >       rec {c4 <- dummyTrack f 100 0 . map noIO $ [return (), mapM_ kill [c,c2,c3,c4] >> kill m >> wr end ()]}
-- >       mapM_ run [c,c2,c3,c4]
-- >       rd end
-- >       hSetBuffering stdout LineBuffering 


