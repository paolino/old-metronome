module System.Metronome.Shortcuts (
	-- * Thread 
		kill
	,	stop
	,	run
	,	modRunning
	-- * Metronome
	,	setTicks
	,	modScheduled
	-- * Track
	,	modPhase
	,	setFrequency
	,	setActions
	,	getPriority
	,	setPriority
	,	modMute
	-- * Dummy
	,	dummyMetronome
	,	dummyTrack
	) where

import Control.Concurrent.STMOrIO
import System.Metronome
import Data.Lens.Lazy


kill,stop,run :: STMOrIO m => Control (Thread a) -> m ()
modRunning ::	STMOrIO m => Control (Thread a) -> (Bool -> Bool) -> m ()

setTicks :: STMOrIO m => Control (Thread Metronome) -> [MTime] -> m ()
modScheduled :: STMOrIO m => Control (Thread Metronome) -> ([(Priority,Actions)] -> ([(Priority, Actions)]) -> m ()

modPhase :: STMOrIO m => Control (Thread Track) -> (Ticks -> Ticks) -> m ()
setFrequency  :: STMOrIO m => Control (Thread Track) -> Frequency -> m ()
setActions :: STMOrIO m => Control (Thread Track) -> [Actions] -> m ()
setPriority :: STMOrIO m => Control (Thread Track) -> [Actions] -> m ()





