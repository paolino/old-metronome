{-# LANGUAGE IncoherentInstances #-}

-- | 
-- Module      :  System.Metronome.Practical
-- Copyright   :  (c) Paolo Veronelli 2012
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  paolo.veronelli@gmail.com
-- Stability   :  unstable
-- Portability :  not portable (requires STM)
--
-- A wrapper module around "System.Metronome" with easy functions.
--
--
-- In this snippet we run a metronome and attach 4 tracks to it. 
--
-- First track ticks every 2 metronome ticks printing \".\" 5 times. 
--
-- Second track ticks at each metronome tick. Forever it reads a string from a variable, 
-- it checks first track for actions finished, and push other 5 actions on the first, each printing the string read.
--
-- Third track ticks every 14 metronome ticks and forever modifies the string in the variable.
--
-- Fourth track ticks every 100 metronome ticks , it does nothing on first action , kill all tracks , including itself and the metronome,
-- and wake up main thread on the second.
-- 
-- > {-# LANGUAGE DoRec #-}
-- > 
-- > import System.IO
-- > import System.Metronome.Practical
-- > import Control.Concurrent.STMOrIO
-- > import Control.Monad
-- > 
-- > main = do
-- >       hSetBuffering stdout NoBuffering
-- >       (m,f) <- dummyMetronome 0.1
-- >       c <- dummyTrack f 2 0 $ replicate 5 $ return $ putStr "."
-- >       v <- var "!"  
-- >       c2 <- dummyTrack f 1 0 . repeat . noIO $ do
-- >                 as <- getActions c
-- >                 vl <- rd v
-- >                 when (null as) . setActions c . replicate 5 . return $ putStr vl
-- >       c3 <- dummyTrack f 14 0 . repeat . noIO . md v $ map succ
-- >       end <- chan ()
-- >       rec {c4 <- dummyTrack f 100 0 . map noIO $ [return (), mapM_ kill [c,c2,c3,c4] >> kill m >> wr end ()]}
-- >       mapM_ run [c,c2,c3,c4]
-- >       rd end
-- >       hSetBuffering stdout LineBuffering 
--
module System.Metronome.Practical (
        -- * Thread 
                kill
        ,        stop
        ,        run
        ,        modRunning
        -- * Metronome
        ,        setTicks
        ,        modScheduled
        -- * Track
        ,        modPhase
        ,        setFrequency
        ,        getActions
        ,        setActions
        ,        setPriority
        ,        modMute
        -- * Dummy
        ,        dummyMetronome
        ,        dummyTrack
        -- * Practical
        ,       noIO 
        ) where

import Prelude hiding ((.),id)
import Control.Category
import Control.Concurrent.STMOrIO
import Control.Concurrent.STM
import Sound.OpenSoundControl
import System.Metronome
import Data.Lens.Lazy

-- | no IO as result of the STM action
noIO :: STM () -> STM (IO ())
noIO f = f >> return (return ())

-- | kill a thread
kill :: STMOrIO m => Control a -> m ()
kill x = md x $ alive ^= False

-- | stop a thread
stop :: STMOrIO m => Control a -> m ()
stop x = md x $ running ^= False

-- | run a thread
run :: STMOrIO m => Control a -> m ()
run x = md x $ running ^= True

-- | invert the running flag of a thread 
modRunning ::        STMOrIO m => Control a -> m ()
modRunning x = md x $ running ^%= not

-- | set the next  ticking times for a metronome
setTicks :: STMOrIO m => Control Metronome -> [MTime] -> m ()
setTicks x = md x . setL (ticks . core)

-- | change the actions scheduled for the next metronome tick
modScheduled :: STMOrIO m => Control Metronome -> ([(Priority,Action)] -> [(Priority, Action)]) -> m ()
modScheduled x = md x . modL (schedule . core) 
 
-- | modify the ticks count from track start, shifting the next ticks relative to metronome ticks
modPhase :: STMOrIO m => Control Track -> (Ticks -> Ticks) -> m ()
modPhase x = md x . modL (sync . core)

-- | set the track frequency
setFrequency  :: STMOrIO m => Control Track -> Frequency -> m ()
setFrequency x = md x . setL (frequency . core) 

-- | set the track actions
setActions :: STMOrIO m => Control Track -> [Action] -> m ()
setActions x = md x . setL (actions . core)

-- | read the remaining actions of a track
getActions :: STMOrIO m => Control Track -> m [Action]
getActions x = (getL $ actions . core) `fmap` rd x

-- | set a track priority
setPriority :: STMOrIO m => Control Track -> Priority -> m ()
setPriority x = md x . setL (priority . core)

-- | mute / unmute a track
modMute ::  STMOrIO m => Control Track  -> m ()
modMute x = md x $ muted . core  ^%= not

-- | create and fork a running metronome.
dummyMetronome  :: MTime        -- ^ time between ticks, in seconds
                 -> IO (Control Metronome, TrackForker) -- ^ metronome control structure and a function to fork tracks
dummyMetronome d = utcr >>= \t0 -> var (Thread True True $ Metronome [t0, t0 + d ..] []) >>= \m -> metronome m >>= \f -> return (m,f)

-- | create and fork a running track by a metronome
dummyTrack      :: TrackForker  -- ^ a track forking action from a metronome fork
                -> Frequency    -- ^ ratio of track ticks and metronome ticks
                -> Priority     -- ^ priority among the track peers
                -> IO (Control Track) -- ^ track control structure
dummyTrack tf fr pr  = var  (Thread True True $ Track 0 fr [] pr False) >>= \t -> tf t >> return t





