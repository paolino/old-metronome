import Graphics.UI.Gtk
import Control.Concurrent
import Data.List 
import Control.Monad (forM_, forM, when)
import Control.Monad.Trans (lift)
import Data.Maybe (catMaybes)
import System.Metronome2
import Control.Monad.Random (getRandomR)
import GHC.Word

push ys x = do 
        let     ys' = reverse ys
                us = zip ys' $ tail ys' 
        forM us $ \(y,y') -> labelGetText y' >>=  labelSetText y
        labelSetText (head ys) x

pop n ys = do
        let (hs,t:_) = splitAt n ys
        labelGetText t >>= push (hs ++ [t])
        
main = do
        initGUI
        b <- builderNew 
        builderAddFromFile b "metronome.glade"
        w <- builderGetObject b castToWindow "value window"
        apply <- builderGetObject b castToButton "apply value"
        synchro <- builderGetObject b castToCheckButton "synchro"
        set <- builderGetObject b castToRadioButton "set operation"
        sum <- builderGetObject b castToRadioButton "sum operation"
        mult <- builderGetObject b castToRadioButton "multiply operation"
        label <- builderGetObject b castToLabel "actual value"
        entry <- builderGetObject b castToEntry "value entry"
        slider <- builderGetObject b castToVScale "value control"
        adj <- builderGetObject b castToAdjustment "control adjustment"
        low <-  builderGetObject b castToButton "low button"
        high <-  builderGetObject b castToButton "high button"
        step <-  builderGetObject b castToButton "step button"
        box <-  builderGetObject b castToHBox "hbox2" 
        (hs,lhs) <- fmap unzip $ forM [1 .. 20] $ \n -> do
                b <- buttonNew 
                l <- labelNew $ Just $ "0"
                containerAdd b l        
                boxPackStart box b PackNatural 0
                return (b,l)
        forM_ (zip [1..] hs) $ \(n,b) -> on b buttonActivated $ pop n $ label : lhs
        on low buttonActivated $ do
                l <- read `fmap` entryGetText entry
                adjustmentSetLower adj l
                
        on high  buttonActivated $ do
                l <- read `fmap` entryGetText entry 
                adjustmentSetUpper adj l
        
        on step buttonActivated $ do
                l <- read `fmap` entryGetText entry
                adjustmentSetStepIncrement adj l
        on entry entryActivate $  buttonClicked apply

        on apply buttonActivated $ do
                
                s <- toggleButtonGetActive set 
                if s then  entryGetText entry >>= push (label : lhs)
                        else do 
                                s <- toggleButtonGetActive sum
                                if s then do 
                                        addendum <- read `fmap` entryGetText entry 
                                        base <- read `fmap` labelGetText label
                                        push (label : lhs) $ show $ (base + addendum :: Double)
                                else do
                                        s <- toggleButtonGetActive mult
                                        if s then do 
                                                factor <- read `fmap` entryGetText entry 
                                                base <- read `fmap` labelGetText label
                                                push (label : lhs) $ show $ (base * factor :: Double)
                                                else return ()

        on synchro toggled $ do 
                s <- toggleButtonGetActive synchro
                when s $ do 
                        toggleButtonSetActive set True
                        buttonClicked apply
        on sum toggled $ do 
                s <- toggleButtonGetActive synchro
                when s $ toggleButtonSetActive synchro False

        on mult toggled $ do 
                s <- toggleButtonGetActive synchro
                when s $ toggleButtonSetActive synchro False
        

        on slider changeValue $ \_ v -> do 
                entrySetText entry (show v)
                s <- toggleButtonGetActive synchro
                when s $ buttonClicked apply
                return False
        widgetShowAll w
        mainGUI


