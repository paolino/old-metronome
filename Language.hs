{-# LANGUAGE TypeFamilies ,  FlexibleContexts, StandaloneDeriving, UndecidableInstances #-}

module Language where



import Data.Ratio
import OpDouble

type SynthName = String
type Controls = [(String,OpDouble)]

type CName = String
type TName = String


type family Change b :: *

data Operation a = A a | M a deriving (Show,Read,Ord,Eq)

data MParameters = DP String | IP String OpDouble | MP String (Operation OpDouble) deriving (Show,Read,Ord,Eq)

data MTrack b = MTPh (Operation Integer) | MTW (Operation Integer) | MTPr (Operation OpDouble) | Mute | Unmute | MTR (Change b) 

deriving instance Show (Change b) => Show (MTrack b)
deriving instance Read (Change b) => Read (MTrack b)
deriving instance Eq (Change b) => Eq (MTrack b)
deriving instance Ord (Change b) => Ord (MTrack b)

data Language b
        = PS SynthName CName 
        | MC CName [MParameters] 
        | MT TName [MTrack b]  
        | CO [Language b] 

deriving instance Show (Change b) => Show (Language b)
deriving instance Read (Change b) => Read (Language b)
deriving instance Eq (Change b) => Eq (Language b)
deriving instance Ord (Change b) => Ord (Language b)

data Sentence b =  NT  TName Integer Integer OpDouble b (Language b)| NP CName Controls 

deriving instance (Show b, Show (Change b)) => Show (Sentence b)
deriving instance (Read b, Read (Change b)) => Read (Sentence b)
deriving instance (Eq b, Eq (Change b)) => Eq (Sentence b)
deriving instance (Ord b, Eq b, Ord (Change b)) => Ord (Sentence b)

type Piece b = [Sentence b]

                
