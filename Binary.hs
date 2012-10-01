{-# LANGUAGE TypeFamilies, ConstraintKinds #-}

module Binary where

import Rythmics
import Control.Monad.State
import Language
import System.Random.Shuffle
import Control.Monad.Random

data Symbol = S | D | J deriving (Show,Read,Eq,Ord)

data Binary = Binary [Symbol] deriving (Show,Read,Eq,Ord)

instance Index Binary where
        -- newL :: Int -> a -> L a
        newL (Binary ns) = evalState (mkL (length ns - 1) ns)  where
                mkL _ [] = do
                        xs <- get
                        case xs of
                                [] -> return $ Pause 1
                                [x] -> return $ Event (1,x)
                                x:xs -> put xs >> return (Event (1,x))
                
                mkL n (J:ns) = liftM2 Mappend (mkL (n - 1) ns)  (mkL (n - 1) ns) 
                mkL n (S:ns) = liftM2 Mappend (mkL (n - 1) ns)  (return . Pause $ fromIntegral $ 2 ^ n)
                mkL n (D:ns) = liftM2 Mappend  (return . Pause $ fromIntegral $ 2 ^ n) (mkL (n - 1) ns)
        readL _ (Merge x y) = error "indexing merge not implemented for Binary"
        readL (Binary []) e@(Event (r,x)) = [e]
        readL (Binary []) _ = [] -- incorrect !
        readL (Binary (S:ns)) (Mappend x y) = readL (Binary ns) x
        readL (Binary (D:ns)) (Mappend x y) = readL (Binary ns) y
        readL (Binary (J:ns)) (Mappend x y) = readL (Binary ns) x ++ readL (Binary ns) y
        readL _ _ = []


        modifyL _ _ (Merge x y) =  error "indexing merge not implemented for Binary"
        modifyL (Binary []) f (Event x) = f $ Event x
        modifyL (Binary (S:ns)) f (Mappend x y) = Mappend (modifyL (Binary ns) f x) y
        modifyL (Binary (D:ns)) f (Mappend x y) = Mappend x (modifyL (Binary ns) f y)
        modifyL (Binary (J:ns)) f (Mappend x y) = Mappend (modifyL (Binary ns) f y) (modifyL (Binary ns) f y)
        modifyL _ f (Pause r) = Pause r
        modifyL _ f (Event x) = Event x


data BinaryChange = Shuffle | Rotate | Select [Int] (Symbol -> Symbol)

type instance RythmChange Binary = BinaryChange

type BCE m = (MonadRandom m, Functor m)

instance ChangeRythm Binary where
        type Env m = BCE m
        changeRythm Shuffle (Binary []) = return $ Binary []
        changeRythm Shuffle (Binary xs) = Binary `fmap` shuffleM xs
