{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ScopedTypeVariables, ExistentialQuantification #-}

module System.Metronome2 where

import Control.Monad (ap, forM_, foldM)
import Data.Monoid (mconcat, mempty, Monoid, mappend)
import Data.List (sortBy, foldl',partition) 
import Data.Ord (comparing)
import Data.Maybe (mapMaybe)
import Data.Ratio (Rational, (%), denominator) 
import Control.Arrow (first, second)
import Data.Maybe (catMaybes)
import Sound.OpenSoundControl (utcr, sleepThreadUntil)
import Data.Map (insert,delete,adjust,Map,empty,keys, findWithDefault, singleton, insertWith, union, mapAccum)
import Control.Concurrent.STM (TVar, atomically, readTVar,writeTVar)
import Debug.Trace

newtype Time = Time Integer deriving (Num, Real, Enum, Integral, Ord, Eq, Show , Read)
newtype Phase = Phase Integer deriving (Num, Real, Enum, Integral, Ord, Eq, Show, Read)
newtype Period = Period Integer deriving (Num, Real, Enum, Integral, Ord, Eq, Show, Read)
newtype Priority = Priority Double deriving (Num, Real, Enum, Ord, Eq, Show, Read)
newtype RealTime = RealTime Double deriving (Num, Real, Enum, Ord, Eq, Fractional, Show,Read)
newtype KeyName b = K b deriving (Eq,Ord,Show,Read)
newtype TrackName b = TN b deriving (Eq,Ord,Show,Read)
newtype ParamName b = PN b deriving (Eq,Ord,Show,Read)

data Scheduling = Infinite | Finite Int deriving (Show,Read) 

reschedule Infinite = Infinite
reschedule (Finite 0) = Finite 0
reschedule (Finite n) = Finite $ n - 1

scheduled (Finite 0) = False
scheduled _ = True

data Pattern = Flat [Rational] | Beat Integer deriving (Show,Read)

fromPattern (Beat n) r  | n `mod` denominator r  == 0 = True
                        | otherwise = False
fromPattern (Flat rs) r = r `elem` rs

data Track b e = Track Pattern Priority Phase Period Scheduling (e b) deriving (Show,Read)

step :: Monoid (e b) => Time -> Track b e -> (Track b e, (Priority, e b))
step t (Track r pr ph pe sc ev) = let
        -- pattern tempo
        t' = (t + fromIntegral ph) `mod` (fromIntegral pe)
        -- update pattern scheduling
        sc' = if t' == 0 then reschedule sc else sc
        -- query scheduling
        b = scheduled sc'
        -- query pattern evenience
        p = fromPattern r $ fromIntegral t' / fromIntegral pe
        in (Track r pr ph pe sc' ev,(pr, if b && p then ev else mempty))



data Operation a = I a | S a | P a | C (Operation a) (Operation a) deriving (Show,Read)

I x `operate` _ = x
S x `operate` y = x + y
P x `operate` y = x * y
C f g `operate` y = f `operate` (g `operate` y)


data Event a b 
        = DN 
        | Mappend (Event a b) (Event a b) 
        | MPr (TrackName b) (Operation Priority)
        | MPh (TrackName b) (Operation Phase) 
        | MPe (TrackName b) (Operation Period) 
        | SPa (TrackName b) Pattern
        | SSc (TrackName b) Scheduling
        | Act a (ParamName b)
        | MS (ParamName b) (KeyName b) (Operation Double) 
        | IS (ParamName b) (KeyName b) Double 
        deriving (Show,Read)


instance Monoid (Event a b) where
        mempty = DN
        mappend = Mappend
------------------------------------------------------------------

type TrackE b a = Track b (Event a)
type Shared b =  Map (ParamName b) (Map (KeyName b) Double)
type TrackMap b a =  Map (TrackName b) (TrackE b a)


data State b a = State { 
        tracks :: TrackMap b a,
        parameters :: Shared b,
        actions :: [(a,ParamName b)]
        }


stepStateTracks :: Time -> TrackMap b a -> (Event a b ,TrackMap b a)
stepStateTracks t = first (mconcat . map snd . sortBy (comparing fst)) . mapAccum f [] where
        f es x = let (x',e) =  step t x in (e:es,x')

digest :: (Ord b, Eq b) 
        => Event a b -> State b a -> (State b a)

digest DN st = st
digest (Mappend e1 e2) t = digest e2 $ digest e1 t
digest (MPr k op) (State ts sh as) = State (adjust f k ts) sh as where
        f (Track pa pr ph pe sc ev) = Track pa (op `operate` pr) ph pe sc ev
digest (MPh k op) (State ts sh as) = State (adjust f k ts) sh as where
        f (Track pa pr ph pe sc ev) = Track pa pr (op `operate` ph) pe sc ev
digest (MPe k op) (State ts sh as) = State (adjust f k ts) sh as where
        f (Track pa pr ph pe sc ev) = Track pa pr ph (op `operate` pe) sc ev
digest (SPa k op) (State ts sh as) = State (adjust f k ts) sh as where
        f (Track pa pr ph pe sc ev) = Track op pr ph pe sc ev
digest (SSc k op) (State ts sh as) = State (adjust f k ts) sh as where
        f (Track pa pr ph pe sc ev) = Track pa pr ph pe op ev
digest (MS p k op) (State ts sh as) = State ts (adjust (adjust (operate op) k) p sh) as
digest (IS p k x) (State ts sh as) =  State ts (insertWith union p (singleton k x) sh) as
digest (Act x p) (State ts sh as) = State ts sh ((x,p):as)


data Metronome b a = Metronome 
        { state :: State b a
        , time :: RealTime
        , futures :: [(Time,RealTime)]
        , messages :: [MM b a] -> Metronome b a
        , next :: IO (Metronome b a)
        }

data MM b a 
        = MME (Event a b) 
        | AT (TrackName b) (TrackE b a) 
        | RT (TrackName b) 
        | MTmM (Operation RealTime) 
        | MPhM (Operation Time) 
        | Sv FilePath 
        | Ld FilePath 
        | Rst (TrackMap b a) (Shared b) deriving (Read,Show)

mkMetronome :: (Eq b, Ord b) => RealTime -> IO (Metronome b a)
mkMetronome d = do
        let     
                nm st rt trts = let m = Metronome st rt trts (mm m) (loop trts st) in m

                loop trts st@(State ts sh as) = do
                        now <- RealTime `fmap` utcr
                        let     ((t,rt):trts') = dropWhile ((<now). snd) $ trts
                                (e, ts') = stepStateTracks t ts
                                st' = digest e (State ts' sh [])
                        sleepThreadUntil $ realToFrac rt
                        return $ nm st' rt trts'
                mm' (Metronome st@(State ts sh as) rt trts _ l) (MME e) = let
                        st' = digest e st
                        in nm st' rt trts
                mm' (Metronome (State ts sh as) rt trts _ l) (AT b t) = nm (State (insert b t ts) sh as) rt trts
                mm' (Metronome (State ts sh as) rt trts _ l) (RT b) = nm (State (delete b ts) sh as) rt trts
                mm' (Metronome st rt trts _ l) (MTmM (S dt)) = nm st rt trts' where
                        trts' = zip [n ..] [t0 + dt, t1 + dt ..]
                        (n,t0):(_,t1):_ = trts
                mm' (Metronome st rt trts _ l) (MTmM (P dt)) = nm st rt trts' where
                        trts' = zip [n ..] [t0 , t0 + (t1 - t0) * dt ..]
                        (n,t0):(_,t1):_ = trts
                mm' (Metronome st rt trts _ l) (MTmM (I t)) = nm st rt trts' where
                        trts' = zip [n ..] [t0 , t0 + t ..]
                        (n,t0):(_,t1):_ = trts
                mm' (Metronome st rt trts _ l) (MPhM (S dp)) = nm st rt trts' where
                        trts' = zip [(n + dp) ..] [t0 , t1 ..]
                        (n,t0):(_,t1):_ = trts
                mm' m (MPhM (P dp)) = m
                mm' (Metronome _  rt trts _ _) (Rst ts sh) = nm (State ts sh []) rt trts
                mm m = foldl' mm' m 
        now <- RealTime `fmap` utcr
        loop (zip [0..][now, now + d ..]) $ State empty empty []
       
run :: (Show b,Ord b, Read b, Read a, Show a) => (RealTime -> Shared b -> [(a,ParamName b)] -> IO ()) -> TVar [MM b a] -> Metronome b a -> IO ()
run f tvs (Metronome (State ts sh as) rt trts mm st) = do 
        -- play
        f rt sh as 
        ms <- atomically (readTVar tvs >>= \x -> writeTVar tvs [] >> return x)
        let     (zs,ms') = partition isMM ms
                isMM (Sv _) = True
                isMM (Ld _) = True
                isMM _ = False
                g m (Sv n) = writeFile n (show (ts,sh)) >> return m
                g m@(Metronome (State ts sh as) rt trts mm l) (Ld n) = do
                        s <- readFile n
                        case reads s of
                                [((ts,sh),_)] -> return (mm [Rst ts sh])
                                _ -> putStrLn ("Parse of " ++ n ++ " failed!") >> return m
        m' <- foldM g (mm ms') zs 
        next m' >>=  run f tvs


        
