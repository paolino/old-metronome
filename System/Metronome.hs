{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}

-- | 
-- Module      :  System.Metronome
-- Copyright   :  (c) Paolo Veronelli 2012
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  paolo.veronelli@gmail.com
-- Stability   :  unstable
-- Portability :  not portable (requires STM)
--
-- /Synchronized execution of sequences of actions, controlled in STM/
--
-- All data structures are made accessible via "Data.Lens" abstraction.
--
-- Actions to be executed are of type 'Action' = STM (IO ()). At each tick, the scheduled actions are ordered by priority, 
-- binded as STM actions ignoring the retrying ones. The results, being IO actions are executed in that order.
--
-- Every 'Track' and 'Metronome' lives in its own thread and can be stopped or killed as such, setting a flag in its state. 
--
-- Track and metronome state are exposed in TVar value to be modified at will. The only closed and inaccessible value is the synchronizing channel, 
-- written by the metronome and waited by tracks.
-- The 'TrackForker' returned by a metronome function is closing this channel and it's the only way to fork a track.
-- 
-- See "System.Metronome.Practical" for an simple wrapper around this module.
--
module System.Metronome  (
        -- * Data structures
                  Track (..)
        ,         Thread (..)
        ,         Metronome (..)
        -- * Lenses
        ,         sync
        ,         frequency
        ,         actions
        ,         priority
        ,         muted
        ,         running
        ,         alive
        ,         core
        ,         ticks
        ,         schedule
        -- * Synonyms
        ,         Control
        ,         Priority
        ,         Frequency
        ,         Ticks
        ,         Action
        ,         MTime
        ,         TrackForker
        -- * API
        ,         metronome
        ) where


import Sound.OpenSoundControl (utcr, sleepThreadUntil)
import Control.Concurrent.STM (STM, TVar, TChan , atomically, newBroadcastTChan, orElse, dupTChan)
import Control.Concurrent (forkIO, myThreadId, killThread)
import Control.Monad (join, liftM, forever, when)
import Data.Ord (comparing)
import Data.List (sortBy)
import Data.Lens.Template (makeLens)
import Data.Lens.Lazy (modL)
import Control.Concurrent.STMOrIO

-- | Track effect interface. Write in STM the collective and spit out the IO action to be executed when all STMs for this tick are done or retried
type Action = STM (IO ())

-- | Priority values between tracks under the same metronome.
type Priority = Double

-- | Number of metronome ticks between two track ticks
type Frequency = Integer

-- | Number of elapsed ticks
type Ticks = Integer

-- execute actions, from STM to IO ignoring retriers
execute :: [Action] -> IO ()
execute = join . liftM sequence_ . atomically . mapM (`orElse` return (return ()))

-- | State of a track.
data Track = Track {        
        -- | the number of ticks elapsed from  the track fork
        _sync :: Ticks,
        -- | calling frequency relative to metronome ticks frequency
        _frequency :: Frequency,
        -- | the actions left to be run
        _actions  :: [Action],
        -- | priority of this track among its peers
        _priority :: Priority,
        -- | muted flag, when True, actions are not scheduled, just skipped
        _muted :: Bool
        }

$( makeLens ''Track)

-- | supporting values with 'running' and 'alive' flag
data Thread a = Thread {
        -- | stopped or running flag
        _running :: Bool,
        -- | set to false to require kill thread
        _alive :: Bool,
        -- | core data
        _core :: a
        }
        
$( makeLens ''Thread)

-- | A Thread value cell in STM
type Control a = TVar (Thread a)

-- | Time, in seconds
type MTime = Double

-- | State of a metronome
data Metronome = Metronome {
        _ticks :: [MTime],        -- ^ next ticking times
        _schedule :: [(Priority, Action)] -- ^ actions scheduled for the tick to come
        }

$( makeLens ''Metronome)

-- | The action to fork a new track from a track state.
type TrackForker = Control Track -> IO ()

-- helper to modify an 'Thread' fulfilling 'running' and 'alive' flags. 
runThread :: (Monad m, RW m TVar) => Control a -> m () -> (a -> m a) -> m ()
runThread  ko  kill modify = do
        -- read the object
        Thread r al x <- rd ko
        if not al then kill 
                else when r $ do 
                        -- modify as requested
                        x' <- modify x 
                        -- write the object
                        md ko $ modL core $ const x'

-- forkIO with kill thread 
forkIO' :: (IO () -> IO ()) -> IO ()
forkIO' f = forkIO (myThreadId >>= f . killThread) >> return ()

--  fork a track based on a metronome and the track initial state
forkTrack :: TChan () -> Control Metronome -> Control Track -> IO ()
forkTrack kc tm tc = forkIO' $ \kill -> do 
        -- make new metronome listener
        kn <- atomically $ dupTChan kc 
        forever $ do
                rd kn -- wait for a tick
                runThread tc kill $ \(Track n m fss z g) -> atomically $ do
                        Thread ru li (Metronome ts ss) <- rd tm
                        -- check if it's time to fire
                        let (ss',fs') = if null fss then (ss,fss) 
                                else let f:fs'' = fss in if n `mod` m == 0 
                                        -- fire if it's not muted
                                        then if not g then ((z,f):ss,fs'') 
                                                -- else don't consume
                                                else (ss,fs'')
                                        else (ss,fss)
                        wr tm $ Thread ru li (Metronome ts ss')
                        -- the new Track with one more tick elapsed and the actions left to run
                        return $ Track (n + 1) m fs' z g



-- | Fork a metronome from its initial state
metronome       :: Control Metronome -- ^ initial state 
                -> IO TrackForker  
metronome km  = do
                kc <- atomically newBroadcastTChan -- non leaking channel
                forkIO' $ \kill ->  forever . runThread km kill $ \m@(Metronome ts _) -> do
                        t <- utcr -- time now
                        -- throw away the past ticking time
                        case dropWhile (< t) ts of
                                [] -> return m  -- no ticks left to wait
                                t':ts' -> do 
                                        -- sleep until next
                                        sleepThreadUntil t'
                                        -- execute scheduled actions after ordering by priority
                                        Metronome _ rs  <- _core `liftM` rd km
                                        execute . map snd . sortBy (comparing fst) $ rs
                                        -- broadcast tick for all track to schedule next actions
                                        wr kc ()
                                        -- the new Metronome with times in future and no actions scheduled 
                                        return $ Metronome ts' [] 
                return $ forkTrack kc km

 




        
             
      
 
