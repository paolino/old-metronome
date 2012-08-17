{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances #-}

-- | 
-- Module      :  Control.Concurrent.STMOrIO
-- Copyright   :  (c) Paolo Veronelli 2012
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  paolo.veronelli@gmail.com
-- Stability   :  unstable
-- Portability :  not portable (requires STM)
--
-- Unifying functions of TVar's and TChans in STM and IO via atomically.
--
module Control.Concurrent.STMOrIO where

import Control.Concurrent.STM

-- | uniforming class for STM or IO
class (Functor m, Monad m) => STMOrIO m where
        stmorio :: STM a -> m a
instance STMOrIO IO where
        stmorio = atomically
instance STMOrIO STM where
        stmorio = id

-- | class to uniform reading and writing 
class RW m z where
        -- | read a z
        rd :: z a -> m a 
        -- | modify a z
        wr :: z a -> a -> m ()

-- | modify a cell z under STM or IO
md :: (Monad m, RW m z) => z a -> (a -> a) -> m ()
md x f = rd x >>= \y -> wr x (f y)

instance STMOrIO t => RW t TVar where
        rd = stmorio . readTVar
        wr x = stmorio . writeTVar x

instance STMOrIO t => RW t TChan where
        rd = stmorio . readTChan
        wr x = stmorio . writeTChan x


-- | new TVar 
var :: STMOrIO m 
        => a                 -- ^ initial value
        -> m (TVar a)
var = stmorio . newTVar

-- | new TChan
chan :: forall m a . STMOrIO m 
        => a                 -- ^ type proxy value
        -> m (TChan a)
chan _ = stmorio (newTChan :: STM (TChan a))


