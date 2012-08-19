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
        ,         core
        ,         ticks
        -- * Synonyms
        ,         Control
        ,         Priority
        ,         Frequency
        ,         Ticks
        ,         Action
        ,         MTime
        -- * API
        ,         metronome
	,	  add 
	,	  delete
	,	  modify
	,	  modifyM
	,	  list
        ) where

import Prelude hiding ((.),id)
import Sound.OpenSoundControl (utcr, sleepThreadUntil)
import Control.Concurrent.STM (STM, TVar, TChan , atomically, newBroadcastTChan, orElse, dupTChan)
import Control.Concurrent (forkIO, myThreadId, killThread, ThreadId)
import Control.Monad (join, liftM, forever, filterM, when)
import Data.Ord (comparing)
import Data.List (sortBy, mapAccumL)
import Data.Maybe (catMaybes)
import Data.Lens.Template (makeLens)
import Data.Lens.Lazy -- (modL, getL, setL)
import Control.Concurrent.STMOrIO
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Category
-- | Time, in seconds
type MTime = Double

-- | Track effect interface. Write in STM the collective and spit out the IO action to be executed when all STMs for this tick are done or retried
type Action = ReaderT MTime STM (IO ())

-- | Priority values between tracks under the same metronome.
type Priority = Double

-- | Number of metronome ticks between two track ticks
type Frequency = Integer

-- | Number of elapsed ticks
type Ticks = Integer


-- | State of a track.
data Track a = Track {   
	-- | track identifier     
	_identifier :: a,
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
        _core :: a
        }
        
$( makeLens ''Thread)

-- | A Thread value cell in STM
type Control a = TVar (Thread a)


-- | State of a metronome
data Metronome a = Metronome {
        _ticks :: [MTime],        -- ^ next ticking times
        _tracks :: [Control (Track a)] -- ^ actions scheduled for the tick to come
        }

$( makeLens ''Metronome)


--  fork a track based on a metronome and the track initial state
step :: Track a -> (Track a, (Priority,Action))
step (Track s n m fs z g) = 
                        -- check if it's time to fire
                        let 	(f,fs')= if null fs then (return $ return (),fs)
                                	else let f:fs' = fs in if n `mod` m == 0 
						then (f,fs') else (return $ return (),fs)
                        	-- the new Track with one more tick elapsed and the actions left to run
                        in (Track s (n + 1) m fs' z g, (z,if not g then return $ return () else f))

stepRunning :: [Control (Track a)] -> IO [Maybe (Priority, Action)]
stepRunning = mapM $ \tx -> atomically $ do 
		x <- rd tx
		let (x',mpa) = if running ^$ x 
			then let (tr,pa) = step $ core ^$ x in (core ^= tr $ x, Just pa)
			else (x,Nothing)
		wr tx x'
		return mpa

--  execute actions, from STM to IO ignoring retriers
execute :: MTime -> [Action] -> IO ()
execute t  = join . liftM sequence_ . atomically  . mapM (\f -> runReaderT f t `orElse` return (return ()))

-- | tick a metronome 
tick :: MTime -> Control (Metronome a) -> IO ()
tick t cm = select cm (const True) >>= stepRunning >>= execute t . map snd . sortBy (comparing fst) . catMaybes

-- ! select tracks from a metronome 
select :: STMOrIO m => Control (Metronome a) -> (a -> Bool) -> m [Control (Track a)]
select cm z = rd cm >>= \m -> filterM (fmap (z . (identifier . core ^$)) . rd) (tracks . core ^$ m) 


-- | Fork a metronome from its initial state
metronome       :: Control (Metronome a) -- ^ initial state 
                -> IO ThreadId
metronome cm  = forkIO . forever $ do
	-- test running state
	run  <- (running ^$) `fmap` rd cm
	when run $ do 
              	t <- utcr -- time now
		mt <- atomically $ do
			-- read next ticks
			ts <- (ticks . core ^$) `fmap` rd cm
			-- throw away the past ticks
			case dropWhile (< t) ts of
				[] -> return Nothing  -- no ticks left to wait
				t':ts' -> do 
					-- update ticks
					md cm (ticks . core ^= ts') 
					-- return next tick
					return $ Just t'
		flip (maybe $ return ()) mt $ \t' ->  do
			-- sleep until next tick
			sleepThreadUntil t' 
			-- execute the tracks at t'
			tick t' cm
                
-- | add a track to a metronome
add :: STMOrIO m =>  Control (Metronome a) -> Control (Track a) -> m ()
add cm ct = md cm $ tracks . core ^%= (ct:)


-- | delete selected tracks from metronome
delete :: STMOrIO m => Control (Metronome a) -> (a -> Bool) -> m ()
delete cm z = do
	ts <- select cm (not . z) 
	md cm $ (tracks . core ^= ts) 

-- | modify selected tracks
modify :: STMOrIO m => Control (Metronome a) -> (a -> Bool) -> (Track a -> Track a) -> m ()
modify cm z f = select cm z >>= mapM_ (flip md $ core ^%= f)

-- | modify selected tracks monadically
modifyM :: STMOrIO m => Control (Metronome a) -> (a -> Bool) -> (Track a -> m (Track a)) -> m ()
modifyM cm z f = select cm z >>= mapM_ (flip mdM $ core ^%%= f)

-- | list selected tracks
list :: STMOrIO m => Control (Metronome a) -> (a -> Bool) -> m [Track a]
list cm z = select cm z >>= mapM (fmap (core ^$) . rd)



        
             
      
 
