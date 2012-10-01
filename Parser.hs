module Parser where

import Prelude hiding (lookup)
import Data.Map hiding (foldr, map)
import Control.Concurrent.STMOrIO 
import Control.Monad (forM, foldM )
import Control.Monad.Trans (lift)
import System.Metronome
import System.Metronome.Practical hiding (delete)
import Rythmics
import Data.Lens.Lazy
import Data.Ratio
import OpDouble


import Language 


data TrackDef b = TrackDef Integer Integer OpDouble b Language deriving (Ord,Show,Read,Eq)

data State b = State (Map TName (TrackDef b)) (Map CName Controls) deriving (Show,Read)

load :: (Ord b,Read b,Show b,Eq b) =>  Piece b -> ([(CName,Controls)],[(TName,TrackDef b)]) 
load = (\(State m1 m2) -> (assocs m2, assocs m1)) . foldr parse (State empty empty)
        where
        parse :: (Ord b,Read b,Show b,Eq b) => Sentence b -> State b -> State b
        parse (NT tn p w pr b l) (State m1 m2) = State (insert tn (TrackDef  p w pr b l) m1) m2
        parse (NP pn p) (State m1 m2) = State m1 (insert pn p m2)

type PMap = Map String OpDouble

updateVars :: (STMOrIO m , Monad m, Functor m) => Map CName (Control PMap) -> (CName, Controls) -> m (Map CName (Control PMap))
updateVars lps      (nv,vs) = let
                mvs = fromList vs :: PMap 
                in
                case nv `lookup` lps of
                        Nothing ->  (\v -> insert nv v lps) `fmap` var mvs
                        Just svs -> do
                                vs' <- rd svs
                                wr svs $ (vs' `union` mvs)
                                return lps
                                

render :: (STMOrIO m , Monad m, Functor m, Index b) 
        => (SynthName -> Control PMap -> Action) -- ^ player
        -> Control (Metronome TName) -- ^ a metronome
        -> [(CName,Controls)]  -- ^ parameters to instantiate
        -> [(TName,TrackDef b)]  -- ^ track to instantiate
        -> Map CName (Control PMap)
        -> m (Map CName (Control PMap)) -- ^ live parameters
render play cm ps ts lps = do
        
        lps' <- foldM updateVars lps ps
        ts' <- forM ts $ \(tn,TrackDef p w pr b l) -> do
                t <- mkTrack tn p w (realToFrac pr) 
                let     resolve (PS sn pn) = return . play sn $ lps' ! pn 
                        resolve (MC pn zs) = return . lift . noIO  $ mapM_ f zs where
                                        f (DP s) = md (lps' ! pn) $ delete s
                                        f (IP s v) = md (lps' ! pn) $ insert s v
                                        f (MP s (A v)) = md (lps' ! pn) $ adjust (+v) s 
                                        f (MP s (M v)) = md (lps' ! pn) $ adjust (*v) s 
                        resolve (MT tn zs) = return . lift . noIO $ mapM_ f zs where
                                        f (MTPh (A v)) = modify cm (==tn) (phase ^%= (+ v))
                                        f (MTPh (M v)) = modify cm (==tn) (phase ^%= (* v))
                                        f (MTW (A v)) = modify cm (==tn) (width ^%= (+ v))
                                        f (MTW (M v)) = modify cm (==tn) (width ^%= (* v))
                                        f (MTPr (A v)) = modify cm (==tn) (priority ^%= (+  realToFrac v))
                                        f (MTPr (M v)) = modify cm (==tn) (priority ^%= (*  realToFrac v))
                                        f Mute = modify cm (==tn) (muted ^= True)
                                        f Unmute = modify cm (==tn) (muted ^= False)
                        resolve (CO xs) = concatMap resolve xs
                md t $ future ^= (newL b $ resolve l)
                return t
        md cm $ tracks ^= ts'
        return lps' 

