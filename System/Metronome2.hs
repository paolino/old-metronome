{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ScopedTypeVariables #-}

module System.Metronome2 where

import Control.Monad (ap, forM_, foldM)
import Data.List (sortBy, foldl',partition) 
import Data.Ord (comparing)
import Data.Maybe (mapMaybe)
import Data.Ratio (Rational, (%)) 
import Sound.OpenSoundControl (utcr, sleepThreadUntil)
import Data.Map (insert,delete,adjust,Map,empty,keys, findWithDefault, singleton, insertWith, union)
import Control.Concurrent.STM (TVar, atomically, readTVar,writeTVar)
newtype Time = Time Integer deriving (Num, Real, Enum, Integral, Ord, Eq, Show , Read)
newtype Phase = Phase Integer deriving (Num, Real, Enum, Integral, Ord, Eq, Show, Read)
newtype Period = Period Integer deriving (Num, Real, Enum, Integral, Ord, Eq, Show, Read)
newtype Priority = Priority Double deriving (Num, Real, Enum, Ord, Eq, Show, Read)
newtype RealTime = RealTime Double deriving (Num, Real, Enum, Ord, Eq, Fractional, Show,Read)


data TrackCtx = TrackCtx Priority Phase Period deriving (Show,Read)
priority (TrackCtx x _ _) = x

data TrackEnv t b  = TrackEnv b TrackCtx Bool t deriving (Show,Read)

identifier (TrackEnv x _ _ _) = x
context (TrackEnv _ x _ _) = x
track (TrackEnv _ _ _ x) = x
mute (TrackEnv _ _ x _) = x
type Track t b a = t -> TrackCtx -> Time -> Event b a

data Operation a = I a | S a | P a | C (Operation a) (Operation a) deriving (Show,Read)

I x `operate` _ = x
S x `operate` y = x + y
P x `operate` y = x * y
C f g `operate` y = f `operate` (g `operate` y)

type Shared b =  Map b (Map b Double)


data Event b a = DN | MPr b (Operation Priority)| MPh b (Operation Phase) | MPe b (Operation Period) | Action a
        | MS (b,b) (Operation Double) | IS (b,b) Double | DS (b,b) | Mu b | UnMu b deriving (Show,Read)


onAction (Action f) = Just f
onAction _ = Nothing

nextEvents ::  Track t b a -> Shared b -> [TrackEnv t b] -> Time -> [Event b a]
nextEvents mkTrack sh ts t = let 
        es = ts >>= \tr -> if mute tr then [] else [(priority (context tr), mkTrack (track tr) (context tr) t)]
        in map snd . sortBy (comparing fst) $ es
        

modifyTrackEnv :: Eq b => [Event b a] -> TrackEnv t b -> TrackEnv t b
modifyTrackEnv es t = foldl' f t es where
        f te@(TrackEnv i (TrackCtx pr ph pe) z t)  (MPr i' op) = check  i i' te (TrackEnv i (TrackCtx (op `operate` pr) ph pe) z t)
        f te@(TrackEnv i (TrackCtx pr ph pe) z t)  (MPh i' op) = check  i i' te (TrackEnv i (TrackCtx pr (op `operate` ph) pe) z t)
        f te@(TrackEnv i (TrackCtx pr ph pe) z t)  (MPe i' op) = check  i i' te (TrackEnv i (TrackCtx pr ph (op `operate` pe)) z t)
        f te@(TrackEnv i (TrackCtx pr ph pe) z t)  (Mu i') = check  i i' te (TrackEnv i (TrackCtx pr ph pe) True t)
        f te@(TrackEnv i (TrackCtx pr ph pe) z t)  (UnMu i') = check  i i' te (TrackEnv i (TrackCtx pr ph pe) False t)
        f te _  = te
        check x y a b = if y == x then b else a


modifyShared :: forall b a . (Eq b, Ord b) => [Event b a] -> Shared b -> Shared b
modifyShared es sh = foldl' f sh es where
        f :: Shared b -> Event b a -> Shared b
        f sh (IS (p,k) x) = insertWith union p (singleton k x) sh
        f sh (DS (p,k)) = adjust (delete k) p sh 
        f sh (MS (p,k) f) = adjust (adjust (operate f) k) p sh 
        f sh _ = sh
        
step :: forall t b a. (Eq b, Ord b) => Track t b a -> Time -> Shared b -> [TrackEnv t b]  -> ([TrackEnv t b], Shared b, [a])
step mkTrack t sh ts = digest sh ts (nextEvents mkTrack sh ts t)
                

digest :: forall t b a. (Eq b, Ord b) =>  Shared b -> [TrackEnv t b]  -> [Event b a] -> ([TrackEnv t b], Shared b, [a])
digest sh ts es = (       map (modifyTrackEnv es) ts 
                ,       modifyShared es sh
                ,       mapMaybe onAction es
                )
data Metronome t b a = Metronome {
          shared :: Shared b
        , tracks :: [TrackEnv t b]
        , actions :: [a]
        , time :: RealTime
        , futures :: [(Time,RealTime)]
        , messages :: [MM t b a] -> Metronome t b a
        , next :: IO (Metronome t b a)
        }

data MM t b a = MME (Event b a) | AT (TrackEnv t b) | RT b | SM | RM | MTmM (Operation RealTime) | MPhM (Operation Time) 
        | Sv FilePath | Ld FilePath | Rst [TrackEnv t b] (Shared b) deriving (Read,Show)

isMM (Sv f) = True
isMM (Ld f) = True
isMM _ = False

mkMetronome :: (Eq b, Ord b) => Track t b a -> RealTime -> [TrackEnv t b] -> IO (Metronome t b a)
mkMetronome mkTrack d ts = do
        let     nm sh ts as rt trts = let m = Metronome sh ts as rt trts (mm m) (loop trts (ts,sh)) in m

                loop trts (ts,sh) = do
                        now <- RealTime `fmap` utcr
                        let     ((t,rt):trts') = dropWhile ((<now). snd) $ trts
                                (ts',sh',as) = step mkTrack t sh ts
                        sleepThreadUntil $ realToFrac rt
                        return $ nm sh' ts' as rt trts'
                mm' (Metronome sh ts as rt trts _ l) (MME e) = let
                        (ts',sh',as') = digest sh ts [e]
                        in nm sh' ts' (as' ++ as) rt trts
                mm' (Metronome sh ts as rt trts _ l) (AT t) = nm sh (t:filter ((/=) (identifier t) . identifier) ts) as rt trts
                mm' (Metronome sh ts as rt trts _ l) (RT b) = nm sh (filter ((/=) b . identifier) ts) as rt trts
                mm' (Metronome sh ts as rt trts _ l) (MTmM (S dt)) = nm sh ts as rt trts' where
                        trts' = zip [n ..] [t0 + dt, t1 + dt ..]
                        (n,t0):(_,t1):_ = trts
                mm' (Metronome sh ts as rt trts _ l) (MTmM (P dt)) = nm sh ts as rt trts' where
                        trts' = zip [n ..] [t0 , t0 + (t1 - t0) * dt ..]
                        (n,t0):(_,t1):_ = trts
                mm' (Metronome sh ts as rt trts _ l) (MTmM (I t)) = nm sh ts as rt trts' where
                        trts' = zip [n ..] [t0 , t0 + t ..]
                        (n,t0):(_,t1):_ = trts
                mm' (Metronome sh ts as rt trts _ l) (MPhM (S dp)) = nm sh ts as rt trts' where
                        trts' = zip [(n + dp) ..] [t0 , t1 ..]
                        (n,t0):(_,t1):_ = trts
                mm' m (MPhM (P dp)) = m
                mm'  (Metronome _ _ _ rt trts _ _) (Rst ts sh) = nm sh ts [] rt trts
                mm m = foldl' mm' m 
        now <- RealTime `fmap` utcr
        loop (zip [0..][now, now + d ..]) (ts, empty)
       

run :: (Show t, Show b,Ord b, Read b, Read t) => (RealTime -> Shared b -> [a] -> IO ()) -> TVar [MM t b a] -> Metronome t b a -> IO ()
run f tvs (Metronome sh ts as rt trts mm st) = do 
        -- play
        f rt sh as 
        ms <- atomically (readTVar tvs >>= \x -> writeTVar tvs [] >> return x)
        let     (zs,ms') = partition isMM ms
                g m (Sv n) = writeFile n (show (ts,sh)) >> return m
                g m@(Metronome sh ts as rt trts mm l) (Ld n) = do
                        s <- readFile n
                        case reads s of
                                [((ts,sh),_)] -> return (mm [Rst ts sh])
                                _ -> putStrLn ("Parse of " ++ n ++ " failed!") >> return m
        m' <- foldM g (mm ms') zs 
        next m' >>=  run f tvs


-----------------------------------------------------------------------------------------
data RTrack b a = RTrack (Event b a) [Rational] deriving (Show, Read)

mkTrack (RTrack e is) (TrackCtx _ ph pe) t = let
               t' = (t + fromIntegral ph) `mod` (fromIntegral pe)
               ts = dropWhile (< t') $ map (Time . floor . (*fromIntegral pe)) is
               in case ts of
                        [] -> DN
                        (t:_) -> if t == t' then e else DN
{-
data StdOut = SOTime | SOPar String String  deriving (Show, Read)

mkAction t s SOTime  = print t
mkAction t s (SOPar p1 p2) = putStrLn $ p1 ++ "." ++ p2  ++ " = " ++ show (findWithDefault 0 p2 $ findWithDefault empty p1 s)

traks = [       TrackEnv "tick" (TrackCtx 0 0 1) False $ (RTrack (Action SOTime) [0])
                ,       TrackEnv "pip" (TrackCtx 0 0 12) False $ (RTrack (Action (SOPar "pip" "amp")) [0%3,1%3,2%3])
                ,       TrackEnv "spip" (TrackCtx 0 0 1) False $ (RTrack (MS ("pip","amp") (S 1)) [0])
                ,       TrackEnv "zpip" (TrackCtx 0 0 10) False $ (RTrack (IS ("pip","amp") 0) [0])
                ]

main = do
        print traks
        m <- mkMetronome mkTrack 0.5 traks
        run (\t s -> mapM_ (mkAction t s)) m
        
-}             
        
