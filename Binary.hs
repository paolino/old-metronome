module Binary where

import Rythmics

data Symbol = S | D | J deriving (Show,Read,Eq,Ord)

data Binary = Binary [Symbol] deriving (Show,Read,Eq,Ord)

instance Index Binary where
        -- newL :: Int -> a -> L a
        newL (Binary ns) = mkL (length ns - 1) ns  where
                mkL _ [] x = Event (1,x)
                mkL n (J:ns) x = Mappend (mkL (n - 1) ns x)  (mkL (n - 1) ns x) 
                mkL n (S:ns) x = Mappend (mkL (n - 1) ns x)  (Pause $ fromIntegral $ 2 ^ n)
                mkL n (D:ns) x = Mappend  (Pause $ fromIntegral $ 2 ^ n) (mkL (n - 1) ns x)
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


