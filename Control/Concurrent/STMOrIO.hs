{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}

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
import Control.Monad.Trans

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
md x f = rd x >>= wr x . f 

-- | modify a cell z under STM or IO, monadically
mdM :: (Monad m, RW m z) => z a -> (a -> m a) -> m ()
mdM x f = rd x >>= f >>= wr x

instance STMOrIO t => RW t TVar where
        rd = stmorio . readTVar
        wr x = stmorio . writeTVar x

instance STMOrIO t => RW t TChan where
        rd = stmorio . readTChan
        wr x = stmorio . writeTChan x

instance (MonadTrans m, STMOrIO t) => RW (m t) TVar where
        rd = lift . rd
        wr x = lift . wr x

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


