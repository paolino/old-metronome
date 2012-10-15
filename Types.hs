{-# LANGUAGE  GeneralizedNewtypeDeriving #-}

module Types where
import Data.Map

newtype Time = Time Integer deriving (Num, Real, Enum, Integral, Ord, Eq, Show , Read)
newtype RealTime = RealTime Double deriving (Num, Real, Enum, Ord, Eq, Fractional, Show,Read)
newtype KeyName b = K b deriving (Eq,Ord,Show,Read)
newtype TrackName b = TN b deriving (Eq,Ord,Show,Read)
newtype ParamName b = PN b deriving (Eq,Ord,Show,Read)

type Shared b =  Map (ParamName b) (Map (KeyName b) Double)
 
