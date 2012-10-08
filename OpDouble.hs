{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module OpDouble where

import Text.Read 
import Text.ParserCombinators.ReadP  hiding ((+++))


newtype OpDouble = OpDouble Double deriving (Show, Num, Real, Ord, Eq)


liftR = readS_to_Prec (const reads)
liftP = readP_to_Prec . const

instance Read OpDouble where
        readPrec = do 
                let f = parens $ do 
                        x <- liftR 
                        liftP skipSpaces
                        liftP $ char '/'
                        liftP skipSpaces
                        y <- liftR
                        return $ x/y
                fmap OpDouble $ liftR +++ f
