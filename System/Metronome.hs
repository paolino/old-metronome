{-# LANGUAGE ExistentialQuantification, Rank2Types #-}

-- | Control the synchronized execution of sequence of actions.
--
-- > Metronome km nc ct <- metronome 0.5
--
-- create a ticking thread at 2 ticks per second
--
-- > ct (*(2/3))
--
-- change the frequency to 3 ticks per second
-- 
-- > Control kc mc sc cc <- nc (Client 2 0 [print 1, print 2, print 3] $ Just 20)
--
-- Start a client which execute an action every 2 ticks of the metronome, with precedence 0 inside this metronome. The 3 print actions are executed one at a time.
-- The sequence is executed 20 times
--
-- > Control kc2 mc2 sc2 cc2 <- nc (Client 3 (-1) [print "a", print "b", print "c"] $ Just 10)
--
-- Start a client which execute an action every 3 ticks of the metronome, with precedence (-1), higher than 0, inside this metronome.
-- When they race, every 6 ticks, letters will be printed before numbers
module System.Metronome where


import Sound.OpenSoundControl
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
import Data.Ord
import Data.List
import Data.Maybe (isJust, fromJust)



-- | Client controls
data Control = Control {
        -- | eliminate client thread
        killC :: IO (),
        -- | mute the client actions, keeping it running
        muteC :: STM (),
        -- | change the phase of the client in the metronome. Negatives are back jumps
        step :: Integer -> STM (),
        -- | add new actions
--        zipIn :: [Action] -> STM (),
        -- | clone this client
        clone :: IO Control
        }

-- | Time between ticks in seconds
type MTime = Double

data Action = AIO {unAIO :: IO ()} | ASTM {unASTM :: STM ()}

execute :: [Action] -> IO ()
execute xs = let (ys,zs) = partition isSTM xs in atomically (sequence_ $ map unASTM ys) >> sequence_ (map unAIO zs) where
        isSTM (ASTM _) = True
        isSTM _  = False
 
-- | Argument to define a new client for a metronome
data Client  = Client 
        {        frequency :: Integer -- ^ calling frequence on metronome ticks
        ,        precedence :: Double -- ^ precedence of this client , negatives higher
        ,        actions     :: [Action] -- ^ sequence of actions to execute
        ,        repetitions :: Maybe Integer -- ^ number of repetitions of the sequence of actions. Nothing loops for ever
        }

-- | Metronome object
data Metronome = Metronome {
        killM :: IO (), -- ^ eliminate this thread with all client threads created on it
        client :: Client -> IO Control , -- ^ attach a new client
        tempo :: (MTime -> MTime) -> STM () -- ^ modify metronome tempo
        }



-- | A fresh  metronome
metronome         :: MTime -- ^ tick tack time
                -> IO Metronome        -- ^ a metronome object
metronome d  = do
        k <- atomically newBroadcastTChan -- a not leaking chan for ticking
        r <- atomically $ newTVar []        --  actions scheduled for next tick 
        ts <- atomically $ newTVar []        -- threads id of attached clients 
        -- instantiation of a client
        let    new mc (Client m z fs mr) = do
                let l = fromIntegral $ length fs -- number of actions , infinite lists demand Nothing in mr
                kn <- atomically $ dupTChan k -- tick listener
                -- state of the client thread, cloned is just resetting counter to modulo m to respect synchro, but reset sequence count, 
                -- muting and functions respected
                kt <- atomically . newTVar $ maybe (0,cycle fs,True) (\(t,fs',g) -> (t `mod` m,fs',g)) mc
                -- client core, recursive to kill the thread
                t <- forkIO . forever $ do  
                                t <- myThreadId
                                join . atomically $ do 
                                        readTChan kn -- wait for a tick
                                        (n,fss@(f:fs''),g) <- readTVar kt -- read state
                                        -- check for end of thread. isJust checked before length for infinite list
                                        if isJust mr && (n `div` (l * m)) == fromJust mr 
                                                then return $ killThread t
                                                else  do 
                                                        -- check if it's time to fire
                                                        fs' <- if n `mod` m == 0 
                                                        -- fire if it's not muted
                                                                then when g (modifyTVar r $ ((z,f):)) >> return fs'' 
                                                                -- else don't consume
                                                                else return fss
                                                        -- update client thread state
                                                        writeTVar kt $ (n + 1, fs',g)
                                                        return $ return ()
                -- add the thread id to the metronome
                atomically . modifyTVar ts $ (t:)
                -- return the control structure for the client
                return $ Control 
                        -- client killer
                        (killThread t>> atomically (modifyTVar ts (delete t))) 
                        -- mute/unmute flipping g
                        (readTVar kt >>= \(c,fs',g) -> writeTVar kt (c,fs',not g))
                        -- dephase time
                        (\n -> readTVar kt >>= \(c,fs',g) -> writeTVar kt (c + n,fs',g))
                        -- clone
                        (atomically (readTVar kt) >>= \c -> new (Just c) (Client m z fs mr))
        -- time now
        t0 <- utcr
        -- schedule next ticks
        kd <- atomically $ newTVar [t0, (t0 + d) ..]
        -- metronome core thread
        t <- forkIO . forever $ do
                -- consume a tick time
                t <- atomically $ do
                        (t:ts') <- readTVar kd
                        writeTVar kd ts'
                        return t
                -- sleep to it
                sleepThreadUntil t
                -- load actions to execute and reset
                rs <- atomically $ do
                        rs <- readTVar r
                        writeTVar r []
                        return rs
                -- execute actions after ordering by precedence
                execute $ map snd . sortBy (comparing fst) $ rs
                                -- tick for all
                atomically $ writeTChan k ()
        -- return the metronome controls
        return $ Metronome 
                -- kill cesar and the filistei
                (killThread t >> atomically (readTVar ts) >>= mapM_ killThread) 
                -- client creation
                (new Nothing)  
                -- rewrite scheduled ticks, from the second to come
                (\f -> do
                        (t1:t2:_) <- readTVar kd
                        writeTVar kd $ [t1,t1 + (f $ t1 - t2) ..]
                        )
        
                




       
 
