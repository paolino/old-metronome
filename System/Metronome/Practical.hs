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
module System.Metronome.Practical {- (
        -- * Thread centered
                stop
        ,       run
        -- * Track centered
        ,         add 
        ,         delete
        ,         modify
        ,         modifyM
        ,         list
        -- * Metronome
        ,        setTicks
        -- * Practical
        ,       mkTrack
        ,       mkMetronome
        ,       noIO 
        ) -}
where

import Prelude hiding ((.),id)
import Control.Category
import Control.Concurrent.STMOrIO
import Control.Concurrent.STM
import Control.Concurrent
import Sound.OpenSoundControl
import System.Metronome
import Data.Lens.Lazy

-- | no IO as result of the STM action
noIO :: STM () -> STM (IO ())
noIO f = f >> return (return ())

-- | stop a thread
stop :: STMOrIO m => Control a -> m ()
stop x = md x $ running ^= False

-- | run a thread
run :: STMOrIO m => Control a -> m ()
run x = md x $ running ^= True


-- | new empty running metronome, given its ticking time
mkMetronome :: MTime -> IO (Control (Metronome a), ThreadId)
mkMetronome d = do 
        t0 <- utcr 
        m <- var (Thread True $ Metronome (zip [0..] [t0, t0 + d ..]) []) 
        t <- forkMetronome m 
        return (m,t)

-- | new standard track, running attached to a metronome
mkTrack :: STMOrIO m => a -> Control (Metronome a) -> Frequency -> Priority -> Rythm -> m (Control (Track a))
mkTrack n m f p as = do
        t <- var (Thread True $ Track n 0 f p [] False [as]) 
        add m t
        return t



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

randevu :: Track a -> Integer
randevu t = (frequency ^$ t) - ((phase ^$ t) `mod` (frequency ^$ t))



futureSyncTrack :: Track a -> [Rythm] -> Track a -> Track a
futureSyncTrack ts as = (phase ^= negate (randevu ts)) . (future ^= as) 

-- | State of a track.
data InfoTrack a = InfoTrack {   
        -- | track identifier     
        _identifier_ :: a,
        -- | the number of ticks elapsed from  the track fork
        _phase_ :: Integer,
        -- | ticks to next event 
        _frequency_ :: Integer,
        -- | priority of this track among its peers
        _priority_ :: Priority,
        -- | the actions left to be run
        _muted_ :: Bool
        } deriving Show

mkInfoTrack :: Track a -> InfoTrack a 
mkInfoTrack t = InfoTrack (identifier ^$ t) (phase ^$ t) (frequency ^$ t) (priority ^$ t) (muted ^$ t) 

info m = list m (const True) >>= return . map mkInfoTrack

mid cm id = modify cm ((==) id)
midM cm id = modifyM cm ((==) id)
sid cm id = select cm ((==) id)

