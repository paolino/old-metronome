{-# LANGUAGE TemplateHaskell, FlexibleContexts, TypeFamilies, MultiParamTypeClasses #-}
module  Schedule where


import Prelude hiding (lookup,Either (..))

import Data.List (maximumBy)
import Data.Ord (comparing)
import Control.Arrow ((&&&), second)
import Control.Concurrent.STM
import Control.Concurrent (forkIO, killThread)
import Control.Monad (forever, join, when)
import Sound.OpenSoundControl (sleepThreadUntil,utcr)






