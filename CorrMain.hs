import Corr
import Data.List
import Data.Ord
import Data.Map (elems, size)
import Control.Arrow (first)
import Cursor
import Schedule
import System.Environment


marks s = [
        Mark s [(1,Sh 0.5),(1,Re $ 7/8)]
        ]

fractals s = [
        Fractal (marks s !! 0) 32 8 64 0
        ]

stats s = map mkFractal (fractals s)

n = 2000
re 0 = "-"
re n = replicate (n `div` 20) ']'
main = do 
        m:n:s:_ <- getArgs
        let (r,v) = point  $ 
                maximumBy (comparing (snd . point)) $ take (read n) $ iterate right (stats (read s)!! read m)
        mapM_ putStrLn $ map re . elems . dist $ r
        print (report $  L (size r)  . map fromIntegral . elems . dist $ r) 
        print (log . fromIntegral $ v)
