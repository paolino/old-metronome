{-# LANGUAGE TypeFamilies, ConstraintKinds #-}

module Binary where

import Rythmics
import Control.Monad.State
import Language
import System.Random.Shuffle
import Control.Monad.Random

data Symbol = S | D | J deriving (Show,Read,Eq,Ord)

data Binary = Binary [Symbol] deriving (Show,Read,Eq,Ord)

normalize :: [Either Integer (Integer,a)] -> R Absolute a
normalize = undefined

instance Index Binary where
        -- newL :: Int -> a -> L a
        mkP (Binary ns) y = normalize $ mkL (length ns - 1) ns  where
                mkL n [] = [Right (2 ^ n, Right y)]
                mkL n (J:ns) = (mkL (n - 1) ns)  ++ (mkL (n - 1) ns) 
                mkL n (S:ns) = (mkL (n - 1) ns)  ++ [Left $ 2 ^ n]
                mkL n (D:ns) = [Left $ 2 ^ n] ++ (mkL (n - 1) ns)
{-

data BinaryChange = Shuffle | Rotate | Select [Int] (Symbol -> Symbol)

type instance RythmChange Binary = BinaryChange

type BCE m = (MonadRandom m, Functor m)

instance ChangeRythm Binary where
        type Env m = BCE m
        changeRythm Shuffle (Binary []) = return $ Binary []
        changeRythm Shuffle (Binary xs) = Binary `fmap` shuffleM xs
-}
