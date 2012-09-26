{-# LANGUAGE TypeFamilies , DatatypeContexts, DataKinds, TypeFamilies, GADTs, KindSignatures #-}

module Language where

import Data.Map hiding (foldl')
import Data.List (foldl')
import Binary
import Control.Concurrent.STMOrIO 
import System.Metronome
import System.Metronome.Practical hiding (delete)
import Control.Monad (forM_ )
import Control.Monad.Trans (lift)
import Rythmics
import Data.Lens.Lazy


type SynthName = String
type Controls = [(String,Double)]

type CName = String
type TName = String

data Operation a = A a | M a deriving (Show,Read,Ord,Eq)

data MParameters = DP String | IP String Double | MP String (Operation Double) deriving (Show,Read,Ord,Eq)

data MTrack = MTPh (Operation Integer) | MTW (Operation Integer) | MTPr (Operation Double) | Mute | Unmute deriving (Show,Read,Ord,Eq)

data Language 
        = PS SynthName CName 
        | MC CName [MParameters] 
        | MT TName [MTrack]  deriving (Ord,Read,Show,Eq)


data Sentence b =  NT  TName Integer Integer Double b | NP CName Controls | Run TName b Language deriving (Ord,Read,Show,Eq)

type Piece b = [Sentence b]

data TrackDef b = TrackDef Integer Integer Double b (Maybe (b,Language)) deriving (Ord,Show,Read,Eq)

data State b = State (Map TName (TrackDef b)) (Map CName Controls) deriving (Show,Read)

parse :: (Ord b,Read b,Show b,Eq b) => Sentence b -> State b -> State b
parse (NT tn p w pr b) (State m1 m2) = State (insert tn (TrackDef  p w pr b Nothing) m1) m2
parse (NP pn p) (State m1 m2) = State m1 (insert pn p m2)
parse (Run tn b' l) (State m1 m2) = State (adjust (\(TrackDef p w pr b _) -> TrackDef p w pr b (Just (b',l))) tn m1) m2

load :: (Ord b,Read b,Show b,Eq b) =>  Piece b -> ([(CName,Controls)],[(TName,TrackDef b)]) 
load = (\(State m1 m2) -> (assocs m2, assocs m1)) . foldl' (flip parse) (State empty empty)

type PMap = Map String Double

boot    :: (STMOrIO m , Monad m, Functor m, Index b) 
        => (SynthName -> Control PMap -> Action) -- ^ player
        -> Control (Metronome TName) -- ^ a metronome
        -> [(CName,Controls)]  -- ^ parameters to instantiate
        -> [(TName,TrackDef b)]  -- ^ track to instantiate
        -> m (Map CName (Control PMap)) -- ^ live parameters
boot play cm ps ts = do
        ps' <- fromList `fmap` mapM (\(n,vs) -> (,) n `fmap` var (fromList vs)) ps
        forM_ ts $ \(tn,TrackDef p w pr b ma) -> do
                t <- mkTrack tn p w pr 
                md t $ future ^= repeat (
                        case ma of 
                                Nothing -> newL b $ return (return ())
                                Just (b,PS sn pn) -> newL b $ play sn $ ps' ! pn 
                                Just (b,MC pn zs) -> newL b $ lift . noIO  $ mapM_ f zs where
                                        f (DP s) = md (ps' ! pn) $ delete s
                                        f (IP s v) = md (ps' ! pn) $ insert s v
                                        f (MP s (A v)) = md (ps' ! pn) $ adjust (+v) s 
                                        f (MP s (M v)) = md (ps' ! pn) $ adjust (*v) s 
                                Just (b,MT tn zs) -> newL b $ lift . noIO $ mapM_ f zs where
                                        f (MTPh (A v)) = modify cm (==tn) (phase ^%= (+v))
                                        f (MTPh (M v)) = modify cm (==tn) (phase ^%= (*v))
                                        f (MTW (A v)) = modify cm (==tn) (width ^%= (+v))
                                        f (MTW (M v)) = modify cm (==tn) (width ^%= (*v))
                                        f (MTPr (A v)) = modify cm (==tn) (priority ^%= (+v))
                                        f (MTPr (M v)) = modify cm (==tn) (priority ^%= (*v))
                                )
                subst cm t
        return ps' 
                
