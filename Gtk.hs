{-# LANGUAGE ExistentialQuantification, TypeSynonymInstances, ScopedTypeVariables, DoRec, Rank2Types #-}
import Graphics.UI.Gtk
import Control.Concurrent
import Control.Monad (forM_, forM, when)
import Control.Monad.Trans (lift)
import Data.Maybe (catMaybes)
import System.Metronome2
import Control.Monad.Random (getRandomR)
import GHC.Word


data Names = GTN Word16 (TrackName String) | GPN Word16 (ParamName String) | GKN Word16 (KeyName String) deriving (Read,Show,Eq,Ord)

showNamesType (GTN n (TN x)) = ((0,n),x)
showNamesType (GPN n (PN x)) = ((1,n),x)
showNamesType (GKN n (K x)) = ((2,n),x)
parseNamesType ((0,n),x) =  (GTN n (TN x))
parseNamesType ((1,n),x) =  (GPN n (PN x))
parseNamesType ((2,n),x) =  (GKN n (K x))

multiPartition :: [Names] -> ([(Word16,TrackName String)],[(Word16,ParamName String)],[(Word16, KeyName String)])

multiPartition [] = ([],[],[])
multiPartition (GTN n k:ks) = let (xs,ys,zs) = multiPartition ks in ((n,k):xs,ys,zs)
multiPartition (GPN n k:ks) = let (xs,ys,zs) = multiPartition ks in (xs,(n,k):ys,zs)
multiPartition (GKN n k:ks) = let (xs,ys,zs) = multiPartition ks in (xs,ys,(n,k):zs)

getBgColor l = rcGetStyle l >>= \s -> styleGetBackground s StateNormal
setBgColor s = widgetModifyBg s StateNormal

packGrow x y = boxPackEnd x y PackGrow 0 
framedIn x = do
        f <- frameNew
        containerAdd f x
        return f

matrix n m = do
        rows  <- vBoxNew True 0
        vbbox <- vBoxNew False 1
        bbox <- hBoxNew False 1
        e <- entryNew
        packNat bbox e
        packNat vbbox bbox
        packGrow  vbbox rows         
        fvbox <- framedIn vbbox

        rec { 
                wss <- forM [1..n] $ \n' -> do
                        cols <- hBoxNew True 0
                        packGrow rows cols  
                        forM [1 .. m] $ \m' -> do 
                                let crs = map showNamesType [GTN 0 (TN ""),GPN 0 (PN ""),GKN 0 (K "")]
                                let i = 3 * (n' - 1) `div` n
                                let ((cr,d),name) = crs !! i
                                lf <- frameNew
                                ll <- labelNew $ Just name
                                l <- eventBoxNew
                                containerAdd lf l
                                containerAdd l ll
                                --eventBoxSetVisibleWindow l True
                                widgetModifyBg l StateNormal (Color (60000 - 25000 * cr) 60000 40000) 
                                on l scrollEvent $ do 
                                        s <- eventScrollDirection
                                        case s of 
                                                ScrollDown -> lift $ do
                                                        Color x y z  <- rcGetStyle l >>= \s -> styleGetBackground s StateNormal
                                                        when (y <= 60000 - 10000) $ widgetModifyBg l StateNormal (Color x (y + 10000) z) 
                                                        return False
                                                ScrollUp -> lift $ do
                                                        Color x y z  <- rcGetStyle l >>= \s -> styleGetBackground s StateNormal
                                                        when (y > 10000) $ widgetModifyBg l StateNormal (Color x (y - 10000) z) 
                                                        return False
                                on l buttonPressEvent $ lift $ do 
                                        t <- entryGetText e
                                        labelSetText ll t
                                        return False
                                packGrow cols lf 
                                return (l,ll)
                }
        let collect = fmap catMaybes . fmap concat . forM [1..n] $ \n' -> forM [1..m] $ \m' -> do 
                l <-  labelGetText $ snd $ wss !! (n' - 1) !! (m' - 1)
                Color x y _ <- rcGetStyle (fst $ wss !! (n' - 1) !! (m' - 1)) >>= \s -> styleGetBackground s StateNormal 
                let     cr = (60000 - x) `div` 25000
                        d = ((60000 - y) `div` 10000)
                return (Just $ parseNamesType ((cr,d),l)) 
        return (fvbox,fmap multiPartition $ collect)

purgeContainer c = containerGetChildren c >>= mapM_ (containerRemove c)
readLabels c = containerGetChildren c >>= mapM (labelGetText . castToLabel)

isW c n =  do
        h <- hBoxNew False 1 
        (_,SetParameter set get) <- parameter (Parameter "valore" (0 :: Double) (const $ return ()) 0 1 0.1) h

        eventConstructor c (Nothing :: Maybe Invisible) g  "Mu" n where
        g (ts,ps,ks) = do
                        p <-  map snd $ filter ((== 1) . fst) ps
                        k <-  map snd $ filter ((== 1) . fst) ks
                        return $ print (IS (p,k) (toDouble x) :: Event String ())

        h <- hBoxNew False 1 
        (_,SetParameter set get) <- parameter (Parameter "valore" (0 :: Double) (const $ return ()) 0 1 0.1) h
        dn <- buttonNewFromStock stockApply
        packGrow h dn  
        on dn buttonActivated $ get $ \x -> do 
                (ts,ps,ks) <- c
                sequence_ $ do
                        p <-  map snd $ filter ((== 1) . fst) ps
                        k <-  map snd $ filter ((== 1) . fst) ks
                        return $ print (IS (p,k) (toDouble x) :: Event String ())

        notebookAppendPage n h "IS"
muteW c n = eventConstructor c (Nothing :: Maybe Invisible) ( \(ts,_,_) -> map (Mu . snd) . filter ((== 1) . fst) $ ts) "Mu" n
unmuteW c n = eventConstructor c (Nothing :: Maybe Invisible) (\(ts,_,_) -> map (UnMu . snd) . filter ((== 1) . fst) $ ts) "UnMu" n
dnW c n = eventConstructor c (Nothing :: Maybe Invisible) (const [DN]) "DN" n 

eventConstructor
  :: (WidgetClass a, NotebookClass n) =>
     IO t
     -> Maybe a 
     -> (t -> [Event String ()]) 
     -> String 
     -> n -> IO Int
eventConstructor c z g s n = do
        b <- hBoxNew True 1
        maybe (return ()) (packNat b) z
        dn <- buttonNewFromStock stockApply
        containerAdd b dn
        vl <- vBoxNew True 1
        containerAdd b vl
        on dn buttonActivated $ do 
                purgeContainer vl
                rs <- c
                forM_ (g rs) $ \c -> do 
                        l <- labelNew $ Just $ show c
                        framedIn l >>= containerAdd vl 
                        widgetShowAll vl
        notebookAppendPage n b s

event c = do
        v <- vBoxNew False 1
        n <- notebookNew
        l <- labelNew $ Just "Event"
        packNat v l
        packGrow  v n  
        fvbox <- frameNew
        containerAdd fvbox v
        dnW c n
        muteW c n
--         unmuteW c n
        isW c n
        return fvbox 
command = do
        b <- vButtonBoxNew 
        l <- labelNew $ Just "Spurious event"
        e <- buttonNew
        containerAdd e l
        containerAdd b e
        return b
main = do
        initGUI
        window <- windowNew
        -- widgetModifyBg window StateNormal (Color 40000 40000 40000) 
        mbox <- hBoxNew True 2
        zbox <- vBoxNew False 1
        cbox <- command
        packNat mbox cbox
        button <- buttonNewFromStock stockClose
        -- packNat zbox mbox
        boxPackEnd  zbox mbox PackNatural 0
        (m,c) <- matrix  8 8
        packGrow  zbox m 
        packNat zbox button
        
        set window [ containerBorderWidth := 10, containerChild := zbox ]
        
        e <- event c
        packGrow  mbox e 
        onDestroy window $ mainQuit
        on button  buttonActivated $ widgetDestroy window 
        
        widgetShowAll window

                
        mainGUI

instance CastDouble Double where
        fromDouble = id
        toDouble = id

instance CastDouble Integer where
        fromDouble = round
        toDouble = fromIntegral


class CastDouble a where
        fromDouble :: Double -> a
        toDouble :: a -> Double

data Parameter = forall a . (CastDouble a, Show a, Read a) => Parameter String a (a -> IO ()) a a a 
data SetParameter = forall a . (CastDouble a, Show a, Read a) => SetParameter (a -> IO ()) ((a -> IO ()) -> IO ())
as :: a -> a -> a
x `as` y = x
parameter :: ContainerClass c => Parameter -> c -> IO (IO (), SetParameter)
parameter (Parameter name s effect (x0::a) x1 dx) c = do
        
        vbox <- vBoxNew False 1 
        fvbox <- frameNew
        containerAdd fvbox vbox
        labelL <- labelNew (Just name)
        label <- labelNew (Just $ show s)
        entry <- entryNew
        check <- checkButtonNew 
        entrySetText entry (show s)
        scale <- vScaleNewWithRange 0 100 1
        rangeSetValue scale (toDouble s)
        button <- buttonNew

        on entry leaveNotifyEvent $ lift $ entryGetText entry >>= \x -> do
                case reads x of 

                        [(y,_)]         -> do
                                                labelSetText label x
                                                rangeSetValue scale (toDouble (y `as` x0))
                        _               -> labelGetText label >>= entrySetText entry

                return False


        on scale changeValue $ const $ \x -> do
                entrySetText entry . show $ (fromDouble $ x) `as` x0
                t <- get check toggleButtonActive 
                when t $ buttonClicked button
                return False
        
        on button buttonActivated $ entryGetText entry >>= labelSetText  label
        
        box <- vBoxNew True 1
        packNat vbox labelL
        packNat box entry
        packNat box button
        packNat box check
        
        packNat box label
        
        hbox <- hBoxNew True 1
--        packNat hbox box
        packGrow  hbox box 

--        packNat vbox hbox
        packGrow  vbox hbox 

        packNat hbox scale
        containerAdd c fvbox
        return (containerRemove c fvbox, 
                SetParameter (postGUIAsync . labelSetText  label . show)
                        (\f -> postGUIAsync (labelGetText label >>= \x -> f (read x `as` x0)) )
                )


packNat x y = boxPackEnd x y PackNatural 0

object :: ContainerClass c => String -> [Parameter] -> c -> IO (IO (), [(String,SetParameter)])
object name sw c = do
        nbox <- vBoxNew False 1
        fvbox <- frameNew
        containerAdd fvbox nbox
        label <- labelNew (Just name) 
        packNat nbox label
        box <- hBoxNew True 1
--         packNat nbox box
        packGrow  nbox box 

        (cs, ps) <- fmap unzip . forM sw $ \param@(Parameter name _ _ _ _ _) -> do 
                (close, s) <-  parameter param box 
                return (close , (name, s))
        containerAdd c fvbox
        return (sequence_ cs >> containerRemove c fvbox, ps) 
        
-- track :: String -> (Double -> IO ()) -> (Integer -> IO ()) -> (Integer -> IO()) -> c -> 


                
        
