import Coo
import Cursor3
import System.Environment
import Control.Concurrent
import System.Random
import Control.Monad
import Data.Array
import Control.Arrow
import System.IO

main = do
        hSetBuffering stdout LineBuffering 
        (n:l:p:w:z:_) <- map read `fmap` getArgs        
         
        ms <- replicateM n $ do 
                s <- randomIO 
                return $ mkWindow s w l p
        let rs = map snd . filter (uncurry (/=)) . ap zip tail . map (toArray . map (normalize . value)) . search  $ ms  
        forkIO $ mapM_ print rs
        threadDelay $ z * 1000000


toArray :: [[Double]] -> Array (Int,Int) Double
toArray xss = array ((0,0),dim) zs where
                zs = [((j,i),x) | (i,y) <- zip [0..] xss, (j,x) <- zip [0..] y] 
                dim = fst $ last zs
