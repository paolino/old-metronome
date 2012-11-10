import Prelude hiding (Either (..))
import Data.List (splitAt)

import Schedule
import Cursor3
import Actions
import Sound.OpenSoundControl
import Control.Concurrent.STM 
import Control.Monad
import Control.Concurrent
import Supercollider4
import Data.Map (empty, fromList)
import Control.Monad.Random hiding (fromList)
{-
newFractal :: Int -> Int -> Int -> Int -> Int -> IO (Window Revol)
newFractal n m op bu ps = do
        s <- getRandom
        let se = mkStdGen s
        return $ flip evalRand se $ do 
                sh <- replicateM n $ getRandomR (0.5,1.5)
                re <-  replicateM n $ getRandomR (0.5,1.5)
                return $ mkRevol s op bu ps $ map Sh sh ++ map (Sh . negate) sh ++ map Re re ++ map (Re . (1/)) re
-}
newFractal :: Int -> Int -> Int -> Int -> Int -> IO (Window Revol)
newFractal n m op bu ps = do
        s <- getRandom
        let se = mkStdGen s
        return $ mkRevol s op bu ps se
--bordone = replicate 8 1 ++ replicate 3 (6/5) ++ replicate 3 (4/3)  ++ replicate 2 (3/2)
bordone_s = replicate 30 0 ++ replicate 20 5 ++ replicate 14 7
bordone_t = bordone_s 
bordone = concat $ replicate 4 [0,2,3,5,7,7,3,2,0,3,7,7,3,0,5,7] 
quarta = map (+ 5) bordone_s
seconda = map (+ 2) bordone_s
quinta = map (+ 7) bordone_s
sesta = map (+ 9) bordone_s
terza = map (+ 4) bordone_s
settima = map (+ 10) bordone_s
ottava = map (+ 12) bordone_s

discese = replicate 32 (d/16) ++ replicate 32 (d/8) 
spider = 15

bars :: [Window Revol] -> Double -> TVar Params -> [Track Window Revol] 
bars stats t0 st = [
        
        (Track spider (times t0 d) $ Bar () (stats !! 11) [renderSParam st (SParam 10 64 "discesa1" $ fromList $ zip [0..] $ discese )]),
        (Track spider (times t0 d) $ Bar () (stats !! 12) [renderSParam st (SParam 10 64 "discesa2" $ fromList $ zip [0..] $ discese )]),
        
        (Track spider (times t0 d) $ Bar () (stats !! 13) [renderTParam st (TParam 64 "attacco1" 0.05 0.01 )]),
        
        (Track spider (times t0 d) $ Bar () (stats !! 10) [renderTParam st (TParam 64 "amp1" 0.45 0.15 )]),
        (Track spider (times t0 d) $ Bar () (stats !! 0) [renderTParam st (TParam 64 "amp2" 0.45 0.15 )]),
        (Track spider (times t0 d) $ Bar () (stats !! 0) [renderTParam st (TParam 64 "amp4" 0.15 0.05 )]),
        (Track spider (times t0 d) $ Bar () (stats !! 3) [renderTParam st (TParam 64 "amp3" 0.4 0.25 )]),
       
        (Track spider (times t0 d) $ Bar () (stats !! 15) [renderSParam st (SParam 10 64 "rate1" $ fromList $ zip [0..63] $ bordone_s)]),
        (Track spider (times t0 d) $ Bar () (stats !! 17) [renderSParam st (SParam 10 64 "rate2" $ fromList $ zip [0..63] $ sesta)]),
        (Track spider (times t0 d) $ Bar () (stats !! 2) [renderSParam st (SParam 10 64 "rate3" $ fromList $ zip [0..63] $ terza)]),
        (Track spider (times t0 d) $ Bar () (stats !! 5) [renderSParam st (SParam 10 64 "rate4" $ fromList $ zip [0..63] $ seconda)]),
        (Track spider (times t0 d) $ Bar () (stats !! 6) [renderSParam st (SParam 10 64 "rate5" $ fromList $ zip [0..63] $ sesta)]),
        (Track spider (times t0 d) $ Bar () (stats !! 20) [renderSParam st (SParam 11 64 "rate7" $ fromList $ zip [0..63] $ ottava)]),
        (Track spider (times t0 d) $ Bar () (stats !! 12) [renderSParam st (SParam 11 64 "rate8" $ fromList $ zip [0..63] $ map (+9) bordone)]),

        (Track spider (times t0 d) $ Bar () (stats !! 1) [renderPlay st scNoteOn (Play 0 64 1 "kick" [("freq","freq"),("discesa","discesa1"),("amp","amp2"),("rate","rate5")] 3)]),
        
        (Track spider (times t0 $ d) $ Bar () (stats !! 9) [renderPlay st scNoteOn (Play 0 64 1 "rullante" [("freq","freq"),("discesa","discesa1"),("amp","amp2"),("rate","rate4")] 3)]),
        

        (Track spider (times t0 d) $ Bar () (stats !! 14) [renderPlay st scNoteOn (Play 6 64 1 "sino" [("freq","freq"),("amp","amp1"),("rate","rate1"),("attacco","attacco1"),("discesa","discesa1")] 3)]),

        (Track spider (times t0 d) $ Bar () (stats !! 16) [renderPlay st scNoteOn (Play 0 64 1 "sino" [("freq","freq"), ("amp","amp1"),("rate","rate4"),("attacco","attacco1"),("discesa","discesa1")] 3)]),

        (Track spider (times t0 d) $ Bar () (stats !! 18) [renderPlay st scNoteOn (Play 0 64 1 "sino" [("freq","freq"),("amp","amp1"),("rate","rate3"),("attacco","attacco1"),("discesa","discesa1")] 3)]),




        (Track spider (times t0 d) $ Bar () (stats !! 19) [renderPlay st scNoteOn (Play 1 64 1 "sino" [("freq","freq"),("amp","amp3"), ("rate","rate7"),("attacco","attacco1"),("discesa","discesa2")]  2)]),

        (Track spider (times t0 d) $ Bar () (stats !! 21) [renderPlay st scNoteOn (Play 0 64 1 "sino" [("freq","freq"),("amp","amp3"),("rate","rate4"),("attacco","attacco1"),("discesa","discesa2")] 2)]),

        (Track spider (times t0 d) $ Bar () (stats !! 22) [renderPlay st scNoteOn (Play 0 64 1 "sino" [("freq","freq"),("amp","amp3"),("rate","rate5"),("attacco","attacco1"),("discesa","discesa2")] 2)]),


        (Track spider (times t0 d) $ Bar () (stats !! 1) [renderPlay st scNoteOn (Play 0 64 1 "sino" [("freq","freq"),("amp","amp4"),("rate","rate8"),("attacco","attacco1"),("discesa","discesa2")] 1)]),

        (Track spider (times t0 (d*8)) $ Bar () (stats !! 24) [renderSParam st (SParam 10 64 "freq" $ fromList $ zip [0..63] $ repeat 45)])

        ]

main = do
        bootSynths
        t0' <- utcr
        ms <- replicateM 25 (newFractal 6 6 20 64 500)
        let     m =    floor $  t0' / d
                t0 = fromIntegral (m + 1) * d
        st <- newTVarIO empty
        (tvs,ts) <- fmap unzip $ forM (bars ms t0 st) $ \t -> newTVarIO t >>= \v -> forkIO (track v) >>= return . (,) v
        let loop = do 
                o <- getLine
                case o of 
                        "re" -> do 
                                let f tv (Track spider _ b) = atomically . modifyTVar tv $ \(Track spider timings _) -> Track spider timings b
                                zipWithM_ f tvs (bars ms t0 st)
                                loop
                        "st" -> return ()
        loop
        mapM killThread ts 

d = 10
times t0 d = map ((t0 +).(*d). fromIntegral) [0..]
{-
main = do 
        t0 <- utcr
        st <- newTVarIO empty
        f <- newFractal 3 3 20 64 1000
        f2 <- newFractal 3 3 20 64 1000
        let     xs = iterate (variate ()) f
                t = Track 5 (times t0 d) $ Bar () f [renderPlay st scNoteOn (Play 0 64 1 "sino" [("amp","amp3")] 0.5)]
                t2 = Track 5 (times t0 d) $ Bar () f2 [renderTParam st (TParam 64 "amp3" 0.4 0.25 )]
        tv <- newTVarIO t
        tv2 <- newTVarIO t2
        forkIO $ track tv
        forkIO $ track tv2
        getLine
        -- mapM_ (print . snd . point) xs 
-}
