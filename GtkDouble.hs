import Graphics.UI.Gtk
import Control.Concurrent
import Control.Monad (forM_, forM, when)
import Control.Monad.Trans (lift)
import Data.Maybe (catMaybes)
import System.Metronome2
import Control.Monad.Random (getRandomR)
import GHC.Word

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
        label <- builderGetObject b castToLabel "value label"
        entry <- builderGetObject b castToEntry "value entry"
        slider <- builderGetObject b castToVScale "value control"
        on apply buttonActivated $ entryGetText entry >>= labelSetText label 
        on synchro toggled $ do 
                s <- toggleButtonGetActive synchro
                when s $ do 
                        toggleButtonSetActive set True
                        entryGetText entry >>= labelSetText label

        on sum toggled $ do 
                s <- toggleButtonGetActive synchro
                when s $ do 
                        toggleButtonSetActive synchro False
                        entryGetText entry >>= labelSetText label

        on slider changeValue $ \_ v -> do 
                entrySetText entry (show v)
                s <- toggleButtonGetActive synchro
                when s $ entryGetText entry >>= labelSetText label
                return False
        widgetShowAll w
        mainGUI


