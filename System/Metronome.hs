{-# LANGUAGE ExistentialQuantification, Rank2Types, TemplateHaskell #-}

-- | Client the synchronized execution of sequence of actions.
--
module System.Metronome where


import Sound.OpenSoundControl (utcr, sleepThreadUntil)
import Control.Concurrent.STM (STM, TVar, TChan , atomically, newTVar, readTVar, readTChan, writeTChan, modifyTVar, newBroadcastTChan, orElse, dupTChan)
import Control.Concurrent (forkIO, myThreadId, killThread)
import Control.Monad (join, liftM, forever, when)
import Data.Ord (comparing)
import Data.List (sortBy)
import Data.Lens.Template
import Data.Lens.Lazy

import Debug.Trace

-- | Client effect interface. Write in STM the collective and spit out the IO action to be executed when all STMs for this tick are done or retried
type Action = STM (IO ())

-- | Priority values between clients under the same metronome.
type Precedence = Double

-- execute actions, from STM to IO ignoring retriers
execute :: [Action] -> IO ()
execute = join . liftM sequence_ . atomically . mapM (`orElse` return (return ()))

-- | state of the client exposed in a TVar to be modified at will.
data Client = Client {        
        -- | the number of ticks elapsed from  the client start
        _sync :: Integer,
        -- | calling frequency relative to ticks frequency
        _frequency :: Integer,
        -- | the next actions scheduled
        _actions :: [Action],
        -- | priority among peers
        _priority :: Precedence,
        -- | muted flag
        _muted :: Bool
        }
$( makeLens ''Client)
-- | supporting values with 'running' and 'alive' flag
data Object a = Object {
        -- | stopped or running
        _running :: Bool,
        -- | set to false to require object gc
        _alive :: Bool,
        -- | core data
        _core :: a
        }
        
$( makeLens ''Object)



mute x = md x $	core ^%= muted ^%= not 
stop x = md x $	running ^= False
kill x = md x $	alive ^= False
run x = md x $ running ^= True

-- | class to uniform IO and STM in reading and modifying TVars
class RdMd m where
        -- | read a TVar
        rd :: TVar a -> m a 
        -- | modify a TVar
        md :: TVar a -> (a -> a) -> m ()
	-- | new TVar
	nv :: a -> m (TVar a)

-- | write a TVar
wr :: RdMd m => TVar a -> a -> m ()
wr x = md x . const 

instance RdMd STM where
        rd = readTVar
        md x = modifyTVar x
	nv = newTVar
instance RdMd IO  where
        rd = atomically . rd
        md x = atomically . md x
	nv = atomically . newTVar

-- | Time between ticks in seconds
type MTime = Double

-- | Metronome is the list of times for the next ticks
data Metronome = Metronome {
        ticks :: [MTime],        -- ^ next ticking times
        schedule :: [(Precedence, Action)], -- ^ actions to be run on next tick
        clock :: TChan ()        -- ^ ticking channel
        }

-- | Make a new metronome  given a ticking time in seconds. This will use a 'BroadcastTChan' which doesn't leak.
mkMetronome :: MTime -> IO Metronome
mkMetronome d = do
        k <- atomically newBroadcastTChan -- a not leaking ochan for ticking
        t0 <- utcr
        return $ Metronome [t0, t0 + d .. ] [] k

-- helper to modify an 'Object' fulfilling 'running' and 'alive' flags. 
runObject :: (Monad m, RdMd m) => TVar (Object a) -> m () -> (a -> m a) -> m ()
runObject  ko  kill modify = do
        -- read the object
        Object running alive x <- rd ko
        if not alive then kill 
                else when running $ do 
                        -- modify as requested
                        x' <- modify x 
                        -- write the object
                        md ko $ modL core $ const x'

-- forkIO for objects  
forkIO' :: (IO () -> IO ()) -> IO ()
forkIO' f = forkIO (myThreadId >>= f . killThread) >> return ()

-- | boot a client based on a metronome and its state
bootClient :: TVar (Object Metronome) -> TVar (Object Client) -> IO ()
bootClient tm tc = forkIO' $ \kill -> do 
        -- read metronome state
        Metronome _ ss kc  <- _core `liftM` rd tm
        -- make new clock listener
        kn <- atomically $ dupTChan kc 
        forever $ do
                atomically $ readTChan kn -- wait for a tick
                runObject tc kill $ \(Client n m fss z g) -> do
                        -- check if it's time to fire
                        let (ss',fs') = if null fss then (ss,fss) 
				else let f:fs'' = fss in if n `mod` m == 0 
                                	-- fire if it's not muted
                                	then if g then ((z,f):ss,fs'') 
                                		-- else don't consume
                                		else (ss,fs'')
					else (ss,fss)
			md tm $ \(Object ru li (Metronome ts ss kc)) -> Object ru li (Metronome ts ss' kc)
                        -- the new Client with one more tick elapsed and the actions left to run
                        return $ Client (n + 1) m fs' z g

-- | boot a metronome from its state. Use 'mkMetronome' to make a standard one. 
bootMetronome :: TVar (Object Metronome) -> IO ()
bootMetronome km  = forkIO' $ \kill ->  forever . runObject km kill $ \m@(Metronome ts _ _) -> do
                t <- utcr -- time now
                -- throw away the past ticking time
                case dropWhile (< t) ts of
                        [] -> return m  -- no ticks left to wait
                        t':ts' -> do 
                                -- sleep until next
                                sleepThreadUntil t'
                                -- execute scheduled actions after ordering by priority
				Metronome _ rs kc <- _core `liftM` rd km
                                execute . map snd . sortBy (comparing fst) $ rs
                                -- broadcast tick for all client to schedule next actions
                                atomically $ writeTChan kc ()
                                -- the new Metronome with times in future and no actions scheduled 
                                return $ Metronome ts' [] kc

 




        
             
      
 
