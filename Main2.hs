
import Prelude hiding (Either (..))
import Data.List (splitAt, tails, nub , delete)

import Schedule
import Cursor3
import Control.Concurrent.STM 
import Control.Monad
import Control.Concurrent
import Control.Monad.Random hiding (fromList)

