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
        ,         Metronome (..)
        -- * Lenses
        -- ** Metronome 
        ,         ticks
        ,         tracks
        -- ** Track
        ,         identifier
        ,         phase
        ,         width 
        ,         actions
        ,         priority
        ,         muted
        ,       future
        -- * Synonyms
        ,         Control
        ,         Priority
        ,         Ticks
        ,         Action
        ,         MTime
        -- * API
        ,         emptyTrack
        ,         forkMetronome
        ,         select
        -- * Test
        ,       runA
        ) where

import Prelude hiding ((.))
import Sound.OpenSoundControl (utcr, sleepThreadUntil)
import Control.Concurrent.STM (STM, TVar,atomically, orElse,retry, newTVar)
import Control.Concurrent (forkIO, ThreadId)
import Control.Monad (join, liftM, forever, filterM, when, guard)
import Data.Ord (comparing)
import Data.List (sortBy, mapAccumL, partition)
import Data.Maybe (catMaybes)
import Data.Lens.Template (makeLens)
import Data.Lens.Lazy ((^=),(^$), (^%=))
import Control.Concurrent.STMOrIO (STMOrIO, rd, wr, md) 
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Category ((.))
import Data.Ratio
import Debug.Trace
import Rythmics

-- | Time, in seconds
type MTime = Double

-- | Track effect interface. Write in STM the collective and spit out the IO action to be executed when all STMs for this tick are done or retried
type Action = ReaderT MTime STM (IO ())

ignore :: Action
ignore = return $ return ()


-- | Priority values to order tracks action.
type Priority = Double


-- | Number of elapsed ticks
type Ticks = Integer

-- | State of a track.
data Track a = Track {   
        -- | track identifier     
        _identifier :: a,
        -- | phase factor 
        _phase :: Ticks,
        _width :: Ticks,
        -- | priority of this track among its peers
        _priority :: Priority,
        -- | the actions left to be run
        _actions  :: P Action,
        -- | muted flag, when True, actions are not scheduled, just skipped
        _muted :: Bool,
        -- | next actions
        _future :: [L Action]
        }

$( makeLens ''Track)

emptyTrack :: a -> Ticks -> Ticks -> Priority -> Track a
emptyTrack i ph w p = Track i ph w p (schedule w 0 $ Pause 1) False []


schedule :: Ticks ->  Ticks -> L Action -> P Action
schedule w c l = mkP (c%1) ((w%1) `mul` normalize l) where

step :: Ticks -> Track a -> (Track a,[(Priority,Action)])
step mc (Track s p w z pas@(P t _) g fs) = let
        c = mc + p
        (P t' as',fs') =  if t <= fromIntegral c then case fs of
                                [] -> (schedule w c $ Pause 1,[]) 
                                (as':fs') -> (schedule w c as',fs') 
                                else (pas,fs)
        (ps,as'') = partition ((<= fromIntegral c) . fst) as'
        tr = Track s p w z (P t' as'') g fs'
        in (tr,if g then [] else zip (repeat z) (map snd ps))


-- | A Thread value cell in STM
type Control a = TVar a

-- | State of a metronome
data Metronome a = Metronome {
        _ticks :: [(Ticks,MTime)],        -- ^ next ticking times
        _tracks :: [Control (Track a)] -- ^ actions scheduled for the tick to come
        }

$( makeLens ''Metronome)

        
-- atomically update all thread tracks
update :: Ticks -> [Control (Track a)] -> STM [(Priority, Action)]
update mc = fmap concat . mapM f where
        f tx = do 
                -- read the track
                x <- rd tx
                -- step it 
                let (x',mpa) = step mc x 
                -- write the new track
                wr tx x'
                -- return actions
                return mpa

--  execute actions, from STM to IO ignoring retriers
execute :: MTime -> [Action] -> IO ()
execute t  = join . liftM (mapM_ forkIO) .  atomically  . mapM (\f -> runReaderT f t `orElse` return (return ()))

-- tick a metronome 
tick :: (Ticks, MTime) -> Control (Metronome a) -> IO ()
tick (mc, t) cm = select cm (const True) >>= atomically . update mc >>= execute t . map snd . sortBy (comparing fst) 


-- | Fork a metronome from its initial state
forkMetronome   :: Control (Metronome a) -- ^ initial state 
                -> IO ThreadId
forkMetronome cm  = forkIO . forever $ do
        -- test running state
                t <- utcr -- time now
                mt <- atomically $ do
                        -- read next ticks
                        ts <- (ticks ^$) `fmap` rd cm
                        -- throw away the past ticks
                        case dropWhile ((< t) . snd ) ts of
                                [] -> return Nothing  -- no ticks left to wait
                                t':ts' -> do 
                                        -- update ticks
                                        md cm (ticks ^= ts') 
                                        -- return next tick
                                        return $ Just t'
                flip (maybe $ return ()) mt $ \t' ->  do
                        -- sleep until next tick
                        sleepThreadUntil (snd t') 
                        
                        -- execute the tracks at t'
                        tick t' cm
                

        
             
-- ! select tracks from a metronome 
select :: STMOrIO m => Control (Metronome a) -> (a -> Bool) -> m [Control (Track a)]
select cm z = rd cm >>= \m -> filterM (fmap (z . (identifier  ^$)) . rd) (tracks  ^$ m) 

-- | test an action
runA :: Action -> IO ()
runA x = do
        t <- utcr
        execute t [x]
      
 
