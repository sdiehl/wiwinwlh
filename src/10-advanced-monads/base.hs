{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad.Base
import Control.Monad.State
import Control.Monad.ST
import Control.Monad.Trans.Control

newtype CounterT m a = CounterT {unCounterT :: StateT Int m a}
  deriving (Functor, Applicative, Monad, MonadTrans)

instance MonadTransControl CounterT where
  type StT CounterT a = StT (StateT Int) a
  liftWith = defaultLiftWith CounterT unCounterT
  restoreT = defaultRestoreT CounterT
