module Parser where

import Data.Map hiding (foldr, map)
import Control.Concurrent.STMOrIO 
import Control.Monad (forM_ , filterM)
import Control.Monad.Trans (lift)
import System.Metronome
import System.Metronome.Practical hiding (delete)
import Rythmics
import Data.Lens.Lazy


import Language 


data TrackDef b = TrackDef Integer Integer Double b Language deriving (Ord,Show,Read,Eq)

data State b = State (Map TName (TrackDef b)) (Map CName Controls) deriving (Show,Read)

load :: (Ord b,Read b,Show b,Eq b) =>  Piece b -> ([(CName,Controls)],[(TName,TrackDef b)]) 
load = (\(State m1 m2) -> (assocs m2, assocs m1)) . foldr parse (State empty empty)
        where
        parse :: (Ord b,Read b,Show b,Eq b) => Sentence b -> State b -> State b
        parse (NT tn p w pr b l) (State m1 m2) = State (insert tn (TrackDef  p w pr b l) m1) m2
        parse (NP pn p) (State m1 m2) = State m1 (insert pn p m2)

type PMap = Map String Double

render :: (STMOrIO m , Monad m, Functor m, Index b) 
        => (SynthName -> Control PMap -> Action) -- ^ player
        -> Control (Metronome TName) -- ^ a metronome
        -> [(CName,Controls)]  -- ^ parameters to instantiate
        -> [(TName,TrackDef b)]  -- ^ track to instantiate
        -> m (Map CName (Control PMap)) -- ^ live parameters
render play cm ps ts = do
        ps' <- fromList `fmap` mapM (\(n,vs) -> (,) n `fmap` var (fromList vs)) ps
        os <- (tracks ^$)  `fmap` rd cm
        os' <- filterM (\ct -> rd ct >>= \t -> return $ (identifier ^$ t) `elem` map fst ts) os
        md cm $ tracks ^= os' 
        forM_ ts $ \(tn,TrackDef p w pr b l) -> do
                t <- mkTrack tn p w pr 
                md t $ future ^= (repeat . newL b) (
                        case l of 
                                PS sn pn -> play sn $ ps' ! pn 
                                MC pn zs -> lift . noIO  $ mapM_ f zs where
                                        f (DP s) = md (ps' ! pn) $ delete s
                                        f (IP s v) = md (ps' ! pn) $ insert s v
                                        f (MP s (A v)) = md (ps' ! pn) $ adjust (+v) s 
                                        f (MP s (M v)) = md (ps' ! pn) $ adjust (*v) s 
                                MT tn zs -> lift . noIO $ mapM_ f zs where
                                        f (MTPh (A v)) = modify cm (==tn) (phase ^%= (+v))
                                        f (MTPh (M v)) = modify cm (==tn) (phase ^%= (*v))
                                        f (MTW (A v)) = modify cm (==tn) (width ^%= (+v))
                                        f (MTW (M v)) = modify cm (==tn) (width ^%= (*v))
                                        f (MTPr (A v)) = modify cm (==tn) (priority ^%= (+v))
                                        f (MTPr (M v)) = modify cm (==tn) (priority ^%= (*v))
                                        f Mute = modify cm (==tn) (muted ^= True)
                                        f Unmute = modify cm (==tn) (muted ^= False)
                                )
                subst cm t
        return ps' 

