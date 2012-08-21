{-# LANGUAGE TemplateHaskell, FlexibleContexts, Rank2Types #-}

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
-- Actions to be executed are of type 'Action' = ReaderT MTime STM (IO ()). The reader gives them access to the scheduling time. They decide how to act and change the common ground in STM and produce an action in IO ().
-- 
-- At each tick, the scheduled actions are ordered by priority, binded as STM actions ignoring the retrying ones. The results, being IO actions are executed in that order.
--
-- 'Track' and 'Metronome' are both 'Thread' and can be run or stopped setting a flag in their state. 
--
-- 'Track' and 'Metronome' state are exposed in a TVar value to be modified at will.
--
-- See "System.Metronome.Practical" for an simple wrapper around this module.
--
module System.Metronome  (
        -- * Data structures
                  Track (..)
        ,         Thread (..)
        ,         Metronome (..)
        -- * Lenses
        -- ** Thread
        ,         running
        ,         core
        -- ** Metronome 
        ,         ticks
        ,         tracks
        -- ** Track
        ,         identifier
        ,         sync
        ,         frequency
        ,         actions
        ,         priority
        ,         muted
        -- * Synonyms
        ,       Duration
        ,         Control
        ,         Priority
        ,         Frequency
        ,         Ticks
        ,         Action
        ,         MTime
        -- * API
        ,         forkMetronome
        ,         select
        ) where

import Prelude hiding ((.))
import Sound.OpenSoundControl (utcr, sleepThreadUntil)
import Control.Concurrent.STM (STM, TVar,atomically, orElse,retry)
import Control.Concurrent (forkIO, ThreadId)
import Control.Monad (join, liftM, forever, filterM, when, guard)
import Data.Ord (comparing)
import Data.List (sortBy)
import Data.Maybe (catMaybes)
import Data.Lens.Template (makeLens)
import Data.Lens.Lazy ((^=),(^$), (^%=))
import Control.Concurrent.STMOrIO (STMOrIO, rd, wr, md) 
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Category ((.))
import Data.Ratio

-- | Time, in seconds
type MTime = Double

-- | Track effect interface. Write in STM the collective and spit out the IO action to be executed when all STMs for this tick are done or retried
type Action = ReaderT MTime STM (IO ())

ignore :: Action
ignore = return $ return ()


-- | Priority values to order tracks action.
type Priority = Double

-- | Number of metronome ticks between two track ticks
type Frequency = Integer

-- | Number of elapsed ticks
type Ticks = Integer

type Duration = Rational


-- | State of a track.
data Track a = Track {   
        -- | track identifier     
        _identifier :: a,
        -- | the number of ticks elapsed from  the track fork
        _sync :: Duration,
        -- | number of ticks to next action
        _frequency :: Frequency,
        -- | priority of this track among its peers
        _priority :: Priority,
        -- | the actions left to be run
        _actions  :: [(Duration,Action)],
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

-- step track sync
tickT = sync ^%= (subtract 1)
resetT x = sync ^= x

-- drop (Functor m, MonadRandom m) =>one track action
dropT = actions ^%= tail

-- update a track state, always add a tick, 
step :: Track a -> (Track a, Maybe (Priority,Action))
step t@(Track _ _ _ _ [] _) = (resetT 0 t, Nothing)
step t@(Track _ n m z ((tk ,f):_) g ) 
        | n <= 0 = (resetT (tk * fromIntegral m - 1) . dropT $ t, guard (not g) >> return (z,f)) 
        | otherwise = (tickT t, Nothing)

-- atomically update a thread track if running
stepRunning :: [Control (Track a)] -> IO [Maybe (Priority, Action)]
stepRunning = mapM $ \tx -> atomically $ do 
                x <- rd tx
                let (x',mpa) = if running ^$ x 
                        then let (tr,mpa') = step $ core ^$ x in (core ^= tr $ x, mpa')
                        else (x,Nothing)
                wr tx x'
                return mpa

--  execute actions, from STM to IO ignoring retriers
execute :: MTime -> [Action] -> IO ()
execute t  = join . liftM (mapM_ forkIO) .  atomically  . mapM (\f -> runReaderT f t `orElse` return (return ()))

-- tick a metronome 
tick :: MTime -> Control (Metronome a) -> IO ()
tick t cm = select cm (const True) >>= stepRunning >>= execute t . map snd . sortBy (comparing fst) . catMaybes

-- ! select tracks from a metronome 
select :: STMOrIO m => Control (Metronome a) -> (a -> Bool) -> m [Control (Track a)]
select cm z = rd cm >>= \m -> filterM (fmap (z . (identifier . core ^$)) . rd) (tracks . core ^$ m) 


-- | Fork a metronome from its initial state
forkMetronome   :: Control (Metronome a) -- ^ initial state 
                -> IO ThreadId
forkMetronome cm  = forkIO . forever $ do
        -- test running state
                t <- utcr -- time now
                mt <- atomically $ do
                        run <- (running ^$) `fmap` rd cm 
                        when (not run) $ retry
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
                

        
             
      
 
