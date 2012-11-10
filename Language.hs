{-# LANGUAGE TypeFamilies ,  FlexibleContexts, StandaloneDeriving, UndecidableInstances #-}

module Language where



import Cursor
import Schedule

        data Operation a = Set a | Sum a | Mult a 


data Language   = Phase (Operation Double) 
                | Width (Operation Double) 
                | SetControl Control
                | ShiftRight Int 
                | ShiftLeft Int
                | ShrinkRight Int
                | ShrinkLeft Int
data RunningTrack = Run (TVar Track) ThreadId | Still Track


startStop ::  RunningTrack -> IO RunningTrack 
startStop (Still t) = do
        r <- var t
        i <- forkTrack s r
        return  $ Run r i
startStop (Run r i) = do
        killThread i
        t <- rd r
        return $ Still r

onTrack f (Still t) = return (Still (f t))
onTrack f (Run r i) = return (Run (atomically $ modifyTVar r f) i)

type TrackName = String

data Playground = Playground {
        _params :: TVar State
        _tracks :: Map TrackName RunningTrack
        }

                
