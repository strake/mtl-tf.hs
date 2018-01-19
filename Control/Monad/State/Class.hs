-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.State.Class
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (multi-param classes, functional dependencies)
--
-- MonadState class.
--
--      This module is inspired by the paper
--      /Functional Programming with Overloading and
--          Higher-Order Polymorphism/,
--        Mark P Jones (<http://www.cse.ogi.edu/~mpj/>)
--          Advanced School of Functional Programming, 1995.

-----------------------------------------------------------------------------

module Control.Monad.State.Class (
    -- * MonadState class
    MonadState(..),
    modify,
    gets,
  ) where

import Control.Monad.Trans.All
import qualified Control.Monad.Trans.All.Strict as Strict
import Control.Monad.Trans.Class

-- ---------------------------------------------------------------------------
-- | /get/ returns the state from the internals of the monad.
--
-- /put/ replaces the state inside the monad.

class (Monad m) => MonadState m where
    type StateType m
    get :: m (StateType m)
    put :: StateType m -> m ()

-- | Monadic state transformer.
--
--      Maps an old state to a new state inside a state monad.
--      The old state is thrown away.
--
-- >      Main> :t modify ((+1) :: Int -> Int)
-- >      modify (...) :: (MonadState Int a) => a ()
--
--    This says that @modify (+1)@ acts over any
--    Monad that is a member of the @MonadState@ class,
--    with an @Int@ state.

modify :: (MonadState m) => (StateType m -> StateType m) -> m ()
modify f = get >>= put . f

-- | Gets specific component of the state, using a projection function
-- supplied.

gets :: (MonadState m) => (StateType m -> a) -> m a
gets f = f <$> get

instance (MonadState m) => MonadState (ContT r m) where
    type StateType (ContT r m) = StateType m
    get = lift get
    put = lift . put

instance (Error e, MonadState m) => MonadState (ErrorT e m) where
    type StateType (ErrorT e m) = StateType m
    get = lift get
    put = lift . put

instance (MonadState m) => MonadState (ListT m) where
    type StateType (ListT m) = StateType m
    get = lift get
    put = lift . put

instance (MonadState m) => MonadState (ReaderT r m) where
    type StateType (ReaderT r m) = StateType m
    get = lift get
    put = lift . put

instance (Monoid w, Monad m) => MonadState (RWST r w s m) where
    type StateType (RWST r w s m) = s
    get   = RWST $ \_ s -> return (s, s, mempty)
    put s = RWST $ \_ _ -> return ((), s, mempty)

instance (Monoid w, Monad m) => MonadState (Strict.RWST r w s m) where
    type StateType (Strict.RWST r w s m) = s
    get   = Strict.RWST $ \_ s -> return (s, s, mempty)
    put s = Strict.RWST $ \_ _ -> return ((), s, mempty)

instance (Monad m) => MonadState (StateT s m) where
    type StateType (StateT s m) = s
    get   = StateT $ \s -> return (s, s)
    put s = StateT $ \_ -> return ((), s)

instance (Monad m) => MonadState (Strict.StateT s m) where
    type StateType (Strict.StateT s m) = s
    get   = Strict.StateT $ \s -> return (s, s)
    put s = Strict.StateT $ \_ -> return ((), s)

instance (Monoid w, MonadState m) => MonadState (WriterT w m) where
    type StateType (WriterT w m) = StateType m
    get = lift get
    put = lift . put

instance (Monoid w, MonadState m) => MonadState (Strict.WriterT w m) where
    type StateType (Strict.WriterT w m) = StateType m
    get = lift get
    put = lift . put
