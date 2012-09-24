{-# LANGUAGE DoRec, StandaloneDeriving,NoMonomorphismRestriction, UndecidableInstances, DeriveTraversable, DeriveFunctor, DeriveFoldable, FlexibleContexts, TypeFamilies , DatatypeContexts #-}

module Supercollider where


import Control.Monad.Random hiding (fromList, MonadRandom)
import Control.Applicative
import Data.Ratio 
import Data.List hiding (insert, concat,all, concatMap,sum)
import Prelude hiding ((.),mapM, concat,all,concatMap,sum)
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
import Sound.SC3.UGen.Noise.ID 
import Sound.OpenSoundControl 
import Data.Lens.Lazy
import Control.Arrow 
import Ritmo
import Data.Random
import Data.Random.Distribution.Categorical (categorical)
import Control.Category ((.))

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
perc = par amp 0.2 * envGen AR 1 1 0 1 RemoveSynth (envPerc (par attacco 0.01) (par discesa 0.5))

percV :: STMOrIO m => m Params
percV = var $ fromList [(attacco,0.01),(discesa,0.5),(amp,0.2)]

type MP = Map String Double
type Params = TVar MP

(*^) :: Int -> [a] -> [a]
n *^ xs = concat $ replicate n xs

inf = (*^) maxBound 

suona :: String -> Params -> Action
suona name fs = rd fs >>= tcs . s_new name (-1) AddToTail 1 . assocs

type R = Ritmo (Bool,Duration)

ritmi :: IO [R]                        
ritmi = do evalRandomIO . iterateM (walk f g) $ Leaf (True,1) where
                f (t,d) = do 
                        mk <- runRVar (categorical [(1::Float, \_ -> Nothing),(if d > 1%32 then 4 else 0,Just)]) StdRandom
                        mk `fmap` replicateM 2 (runRVar (categorical  [(1::Float,(False,d / 2)),(1.5,(True,d / 2))]) StdRandom)
                g ds =  do
                        p <- runRVar (categorical  [(1::Float,False),(2,True)]) StdRandom
                        
                        runRVar (categorical [(if snd (head ds) > 1%4 then 0 else 1, Just (p,sum $ map snd ds)),(1::Float,Nothing)]) StdRandom 
        

addPause :: (Applicative m, MonadRandom m) => Ritmo Duration -> m (Ritmo (Bool,Duration))
addPause =  traverse (\x -> runRVar (categorical  [(1::Float,(False,x)),(2,(True,x))]) StdRandom)
bootBinary ntracks pace subd = do
        let ticks = 2 ^ subd
        (m,kt) <- mkMetronome $ pace/ fromIntegral ticks 
        ts <- mapM (\i -> mkTrack i m ticks (ntracks - i) []) [1 .. ntracks]
        return (m,kt,ts)


boot = do 
        addGroup1
        let s r =  klankSpec ([r 80,120,158]) [1,1,1,1] [1,1,1,1]
        addSynth "klank" $ dynKlank (impulse AR 2 0 * 0.4) 1 0 1 (s (* control KR "rate" 1)) * perc 
        addSynth "noise" $ whiteNoise 'a' AR * perc 
        addSynth "s" $ sine "f1" * sine "f2" * perc


applica :: (Duration -> Action) -> R -> Schedule
applica f = map (\(t,x) -> (x,if t then lift . noIO $ return () else f x)) . toList

mo x f = lift . noIO $ md x f

piece = do 
        boot
        (m,km,[t1,t2,t3,t4,t5]) <- bootBinary 5 4 7 
        v <- percV
        map (applica (\x -> suona "klank" v)) `fmap` ritmi >>= md t1 . (future . core ^=)
        map (applica (\x -> lift . noIO . md v $ insert "rate" $ 1 + fromRational x)) `fmap` ritmi >>= md t3 . (future . core ^=)
        map (applica (\x -> lift . noIO . md v $ insert "amp" $ min 0.5 (8*fromRational x))) `fmap` ritmi >>= md t2 . (future . core ^=)
        return $ killThread km
        
{-
t1 t1 v = schedule (const $ suona "bass" v) v 2 15 >>= \ts -> md t1 (future . core ^= inf [ts])
t2 t2 v = schedule (\x -> mo v $ insert "amp" (0.1 + fromRational x)) v 4 40 >>= \ts -> md t2 (future . core ^= inf [ts])
t3 t3 v =  schedule (\x -> mo v $ insert "rate" (1 + fromRational x)) v 5 50 >>= \ts -> md t3 (future . core ^= inf [ts])
syncem t1 t2 = atomically $ do
        s <- rd t2 $ System.Metronome.sync . core 
        md t1 $ System.Metronome.sync . core ^= s
        
-}
{-
(m,km,[t1,t2,t3,t4,t5]) <- bootBinary 5 1 7 
v <- percV
md t1 $ future . core ^= [4 *^ [(1%16,suona "s" v)], 4 *^ [(1%16,suona "s" v)] ]

type Scatter m = Double -> Int -> m [Double]

scatterR :: (Functor m, MonadRandom m) => Double -> Int -> m [Double]
scatterR l n = do
        xs <- (sort . take (n - 1))  `fmap` getRandomRs (0,l)
        return $ zipWith (flip (-)) (0:xs) $ xs ++ [l]


scatterS l n = return $ replicate n $ l/fromIntegral n

evalScatter :: (Functor m, MonadRandom m) => Scatter m -> Rational -> Double -> Ritmo a -> m [Either Rational (Rational,Double)]
evalScatter _ d p Play = return [Right (d,p)]
evalScatter _ d p Pause = return [Left d]
evalScatter scatter d p (Subd n xs) = do
        ps <- scatter p n 
        fmap concat . mapM (uncurry $ evalScatter scatter (d/fromIntegral n)) $ zip ps xs


	




stringMetronome :: MTime -> IO (Control (Metronome String), ThreadId)
stringMetronome = mkMetronome


isSubd (Subd _ _) = True
isSubd _ = False


randomCollapse :: (MonadRandom m, Functor m) => Ritmo -> m Ritmo
randomCollapse Pause = return Pause
randomCollapse Play = return Play
randomCollapse (Subd m xs) 
        | all (not . isSubd) xs = pp `fmap` getRandom
        | otherwise = do
                let (rs,is) = partition (isSubd . snd) $ zip [0 ..] xs
                n <- getRandomR (0, length rs -1)
                let (rs',r:rs'') = splitAt n rs
                r' <- randomCollapse (snd r)
                return $ Subd m . map snd $ sortBy (comparing fst) $ (n,r'):rs'++ rs'' ++ is

-}

