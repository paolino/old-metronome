{-# LANGUAGE TypeFamilies , DatatypeContexts, DataKinds, TypeFamilies, GADTs, KindSignatures #-}

module Language where




type SynthName = String
type Controls = [(String,Double)]

type CName = String
type TName = String

data Operation a = A a | M a deriving (Show,Read,Ord,Eq)

data MParameters = DP String | IP String Double | MP String (Operation Double) deriving (Show,Read,Ord,Eq)

data MTrack = MTPh (Operation Integer) | MTW (Operation Integer) | MTPr (Operation Double) | Mute | Unmute deriving (Show,Read,Ord,Eq)

data Language 
        = PS SynthName CName 
        | MC CName [MParameters] 
        | MT TName [MTrack]  deriving (Ord,Read,Show,Eq)


data Sentence b =  NT  TName Integer Integer Double b Language | NP CName Controls deriving (Ord,Read,Show,Eq)

type Piece b = [Sentence b]

                
