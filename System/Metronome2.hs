{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ScopedTypeVariables #-}

module System.Metronome2 where

import Control.Monad (ap)
import Data.List (sortBy, foldl') 
import Data.Ord (comparing)
import Data.Maybe (mapMaybe)
import Data.Ratio (Rational, (%)) 
import Sound.OpenSoundControl (utcr, sleepThreadUntil)
import Data.Map (insert,delete,adjust,Map,empty,keys, findWithDefault, singleton, insertWith, union)

newtype Time = Time Integer deriving (Num, Real, Enum, Integral, Ord, Eq, Show , Read)
newtype Phase = Phase Integer deriving (Num, Real, Enum, Integral, Ord, Eq, Show, Read)
newtype Period = Period Integer deriving (Num, Real, Enum, Integral, Ord, Eq, Show, Read)
newtype Priority = Priority Double deriving (Num, Real, Enum, Ord, Eq, Show, Read)
newtype RealTime = RealTime Double deriving (Num, Real, Enum, Ord, Eq, Fractional, Show,Read)


data TrackCtx = TrackCtx {
                priority :: Priority
        ,       phase :: Phase
        ,       period :: Period
        } deriving (Show,Read)

data TrackEnv t b  = TrackEnv {
                identity :: b
        ,       context :: TrackCtx
        ,       mute :: Bool
        ,       track :: t
        } deriving (Show,Read)

type Track t b a = t -> TrackCtx -> Time -> Event b a

data Operation a = S a | P a | C (Operation a) (Operation a) deriving (Show,Read)

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
step mkTrack t sh ts = let es = nextEvents mkTrack sh ts t
        in      (       map (modifyTrackEnv es) ts 
                ,       modifyShared es sh
                ,       mapMaybe onAction es
                )
        
data Metronome t b a = Metronome {
          shared :: Shared b
        , tracks :: [TrackEnv t b]
        , actions :: [a]
        , time :: RealTime
        , next :: IO (Metronome t b a)
        }


mkMetronome :: (Eq b, Ord b) => Track t b a -> RealTime -> [TrackEnv t b] -> IO (Metronome t b a)
mkMetronome mkTrack (RealTime d) ts = do
        now <- utcr
        let loop trts (ts,sh) = do
                        now <- utcr
                        let     ((t,rt):trts') = dropWhile ((<now). snd) $ trts
                                (ts',sh',as) = step mkTrack (Time t) sh ts
                        sleepThreadUntil rt
                        return $ Metronome sh' ts' as (RealTime rt) (loop trts' (ts',sh')) 
        loop (zip [0..][now, now + d ..]) (ts, empty)
       

run :: (RealTime -> Shared b -> [a] -> IO ()) -> Metronome t b a -> IO ()
run f (Metronome sh ts as rt st) = f rt sh as >> st >>= run f 


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
        
