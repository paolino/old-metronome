{-# LANGUAGE TypeFamilies , DatatypeContexts, DataKinds, TypeFamilies, GADTs, KindSignatures, FlexibleContexts, StandaloneDeriving, UndecidableInstances, IncoherentInstances, ConstraintKinds #-}

module Language where



import Data.Ratio
import OpDouble
import GHC.Prim

type SynthName = String
type Controls = [(String,OpDouble)]

type CName = String
type TName = String


type family RythmChange b :: *

class ChangeRythm b where
        type Env (m :: (* -> *)) :: Constraint
        changeRythm :: Env m => RythmChange b -> b -> m b

data Operation a = A a | M a deriving (Show,Read,Ord,Eq)

data MParameters = DP String | IP String OpDouble | MP String (Operation OpDouble) deriving (Show,Read,Ord,Eq)

data MTrack b = MTPh (Operation Integer) | MTW (Operation Integer) | MTPr (Operation OpDouble) | Mute | Unmute | MRT (RythmChange b) 

deriving instance Show (RythmChange b) => Show (MTrack b)
deriving instance Read (RythmChange b) => Read (MTrack b)
deriving instance Eq (RythmChange b) => Eq (MTrack b)
deriving instance Ord (RythmChange b) => Ord (MTrack b)

data Language b
        = PS SynthName CName 
        | MC CName [MParameters] 
        | MT TName [MTrack b]  
        | CO [Language b] 

deriving instance Show (RythmChange b) => Show (Language b)
deriving instance Read (RythmChange b) => Read (Language b)
deriving instance Eq (RythmChange b) => Eq (Language b)
deriving instance Ord (RythmChange b) => Ord (Language b)

data Sentence b =  NT  TName Integer Integer OpDouble b (Language b)| NP CName Controls 

deriving instance (Show b, Show (RythmChange b)) => Show (Sentence b)
deriving instance (Read b, Read (RythmChange b)) => Read (Sentence b)
deriving instance (Eq b, Eq (RythmChange b)) => Eq (Sentence b)
deriving instance (Ord b, Eq b, Ord (RythmChange b)) => Ord (Sentence b)

type Piece b = [Sentence b]

                
