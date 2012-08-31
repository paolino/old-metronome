{-# LANGUAGE DoRec, StandaloneDeriving,NoMonomorphismRestriction, UndecidableInstances, DeriveTraversable, DeriveFunctor, DeriveFoldable, FlexibleContexts, TypeFamilies , DatatypeContexts #-}

module Supercollider where


import Control.Monad.Random hiding (fromList)

import Data.Ratio 
import Data.List hiding (insert, concat,all)
import Prelude hiding ((.), mapM, concat,all)
import Data.Map (Map, insert, fromList, assocs, update, adjust)
import Data.Monoid
import Data.Traversable
import Data.Foldable
import Control.Monad.Reader hiding (mapM)
import Data.Ord (comparing)
import System.Metronome.Practical
import System.Metronome
import System.IO.Unsafe (unsafePerformIO)
import System.Metronome (Action, MTime, Schedule)
import Control.Concurrent.STMOrIO
import Control.Concurrent.STM
import Control.Concurrent
import Sound.SC3 -- (withSC3, send, g_new, d_recv, synthdef, out , mce, control, Rate (..), sinOsc, envGen, DoneAction (..), envPerc, s_new, AddAction (AddToTail))
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
attacco = "attacco"
discesa = "discesa"
-- parametric sine
sine f = sinOsc AR (par f 220) 0 
perc = par amp 0.2 * envGen AR 1 1 0 1 RemoveSynth (envPerc (par attacco 0.01) (par discesa 0.5))

percV :: STMOrIO m => m Params
percV = var $ fromList [(attacco,0.01),(discesa,0.5),(amp,0.2)]

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

-- a suonaer
suona :: String -> Params -> Action
suona name fs = rd fs >>= tcs . s_new name (-1) AddToTail 1 . assocs
                        


data Ritmo a = Play a | Pause a | Split Int [Ritmo a] deriving (Show,Traversable, Functor, Foldable)
data Or a = L a | R a deriving (Show,Functor)

orring f g (L x) = f x
orring f g (R x) = g x

leaf d True = Play (R d)
leaf d False = Pause (L d) 
leafN k d n
        | n < k = Play (R d)
        | otherwise = Pause (L d)
type R = Ritmo (Or Rational)

randomSplit :: (MonadRandom m, Functor m) => Int -> R -> m R
randomSplit _ (Pause (L d)) = fmap (leafN 4 d) $ getRandomR (1::Int,4)
randomSplit n (Play (R d)) = fmap (Split n) . fmap (map (leaf $ d / fromIntegral n)) $ replicateM n getRandom
randomSplit n (Split m xs) = do 
        k <- getRandomR (0,m - 1)
        let (bs,x:cs) = splitAt k xs
        (Split m . (bs ++) . (++ cs) . return) `fmap` randomSplit n x 
                 
randomRitmo :: (Functor m, MonadRandom m) => Rational -> Int -> m R
randomRitmo k n = foldM (\x _ -> randomSplit 2 x) (leaf k True) [1..n] 

{-

type Scatter m = Double -> Int -> m [Double]

scatterR :: (Functor m, MonadRandom m) => Double -> Int -> m [Double]
scatterR l n = do
        xs <- (sort . take (n - 1))  `fmap` getRandomRs (0,l)
        return $ zipWith (flip (-)) (0:xs) $ xs ++ [l]


scatterS l n = return $ replicate n $ l/fromIntegral n

evalScatter :: (Functor m, MonadRandom m) => Scatter m -> Rational -> Double -> Ritmo a -> m [Either Rational (Rational,Double)]
evalScatter _ d p Play = return [Right (d,p)]
evalScatter _ d p Pause = return [Left d]
evalScatter scatter d p (Split n xs) = do
        ps <- scatter p n 
        fmap concat . mapM (uncurry $ evalScatter scatter (d/fromIntegral n)) $ zip ps xs


	




stringMetronome :: MTime -> IO (Control (Metronome String), ThreadId)
stringMetronome = mkMetronome


isSplit (Split _ _) = True
isSplit _ = False


randomCollapse :: (MonadRandom m, Functor m) => Ritmo -> m Ritmo
randomCollapse Pause = return Pause
randomCollapse Play = return Play
randomCollapse (Split m xs) 
        | all (not . isSplit) xs = pp `fmap` getRandom
        | otherwise = do
                let (rs,is) = partition (isSplit . snd) $ zip [0 ..] xs
                n <- getRandomR (0, length rs -1)
                let (rs',r:rs'') = splitAt n rs
                r' <- randomCollapse (snd r)
                return $ Split m . map snd $ sortBy (comparing fst) $ (n,r'):rs'++ rs'' ++ is

ritmoStream x = do
        x' <- randomSplit 2 x
        x'' <- randomCollapse x'
        xs <- ritmoStream x''
        return $ x' : x'' : xs

-}
-- randomRitmo2 scatterS 30 10
bootBinary ntracks pace subd = do
        let ticks = 2 ^ subd
        (m,kt) <- mkMetronome $ pace/ fromIntegral ticks 
        ts <- mapM (\i -> mkTrack i m ticks (ntracks - i) []) [1 .. ntracks]
        return (m,kt,ts)


boot = do 
        addGroup1
        cs $ b_allocRead 1 "/home/paolino/Music/bass.wav" 0 0
        let s r =  klankSpec (map r [80,120,158]) [1,1,1,1] [1,1,1,1]
        addSynth "klank" $ dynKlank (impulse AR 2 0 * 0.1) 1 0 1 (s (* control KR "rate" 1)) * perc 
        addSynth "s" $ sine "f1" * sine "f2" * perc
        addSynth "bass" $ playBuf 2 AR 1 (control KR "rate" 1) 0 0 NoLoop RemoveSynth * perc

pausa = lift . noIO $ return ()
mo x  = lift . noIO . md x 
schedule a v ns t = map (orring (\x -> (x,pausa)) (\x -> (x,a x))) `fmap` toList `fmap` randomRitmo ns t :: IO Schedule
t1 t1 v = schedule (const $ suona "bass" v) v 2 15 >>= \ts -> md t1 (future . core ^= inf [ts])
t2 t2 v = schedule (\x -> mo v $ insert "amp" (0.1 + fromRational x)) v 4 40 >>= \ts -> md t2 (future . core ^= inf [ts])
t3 t3 v =  schedule (\x -> mo v $ insert "rate" (1 + fromRational x)) v 5 50 >>= \ts -> md t3 (future . core ^= inf [ts])

syncem t1 t2 = atomically $ do
        s <- rd t2 $ System.Metronome.sync . core 
        md t1 $ System.Metronome.sync . core ^= s
        
{-
(m,km,[t1,t2,t3,t4,t5]) <- bootBinary 5 1 7 
-}
{-
v <- percV
md t1 $ future . core ^= [4 *^ [(1%16,suona "s" v)], 4 *^ [(1%16,suona "s" v)] ]

audition $ out 0 $ suonaBuf 2 AR 1 (control KR "rate" 1) 0 0 NoLoop RemoveSynth * (control KR "amp" 1)


-}
-- > {-# LANGUAGE DoRec #-}




-- > 
-- > import System.IO
-- > import System.Metronome.Practical
-- > import Control.Concurrent.STMOrIO
-- > import Control.Monad
-- > ghc force garbage collection
-- > main = do schedule (\x -> mo v $ insert "rate" (1 + fromRational x)) v 5 50 >>= \ts -> md t3 (future . core ^= inf [ts])
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


