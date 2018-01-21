{-# LANGUAGE UndecidableInstances #-}
{- |
Module      :  Control.Monad.Reader.Class
Copyright   :  (c) Andy Gill 2001,
               (c) Oregon Graduate Institute of Science and Technology 2001,
               (c) Jeff Newbern 2003-2007,
               (c) Andriy Palamarchuk 2007
License     :  BSD-style (see the file libraries/base/LICENSE)

Maintainer  :  libraries@haskell.org
Stability   :  experimental
Portability :  non-portable (multi-param classes, functional dependencies)

[Computation type:] Computations which read values from a shared environment.

[Binding strategy:] Monad values are functions from the environment to a value.
The bound function is applied to the bound value, and both have access
to the shared environment.

[Useful for:] Maintaining variable bindings, or other shared environment.

[Zero and plus:] None.

[Example type:] @'Reader' [(String,Value)] a@

The 'Reader' monad (also called the Environment monad).
Represents a computation, which can read values from
a shared environment, pass values from function to function,
and execute sub-computations in a modified environment.
Using 'Reader' monad for such computations is often clearer and easier
than using the 'Control.Monad.State.State' monad.

  Inspired by the paper
  /Functional Programming with Overloading and
      Higher-Order Polymorphism/, 
    Mark P Jones (<http://www.cse.ogi.edu/~mpj/>)
    Advanced School of Functional Programming, 1995.
-}

module Control.Monad.Reader.Class (
    MonadReader(..),
    asks,
    ) where

import Control.Monad.Trans.All
import qualified Control.Monad.Trans.All.Strict as Strict
import Control.Monad.Trans.Class

{- |
See examples in "Control.Monad.Reader".
Note, the partially applied function type @(->) r@ is a simple reader monad.
See the @instance@ declaration below.
-}
class (Monad m) => MonadReader m where
    type EnvType m

    -- | Retrieves the monad environment.
    ask   :: m (EnvType m)
    {- | Executes a computation in a modified environment. Parameters:

    * The function to modify the environment.

    * @Reader@ to run.

    * The resulting @Reader@.
    -}
    local :: (EnvType m -> EnvType m) -> m a -> m a

{- |
Retrieves a function of the current environment. Parameters:

* The selector function to apply to the environment.

See an example in "Control.Monad.Reader".
-}
asks :: (MonadReader m) => (EnvType m -> a) -> m a
asks f = f <$> ask

instance (Monad m) => MonadReader (ReaderT r m) where
    type EnvType (ReaderT r m) = r
    ask       = ReaderT return
    local f m = ReaderT $ \r -> runReaderT m (f r)

-- ----------------------------------------------------------------------------
-- The partially applied function type is a simple reader monad

instance MonadReader ((->) r) where
    type EnvType ((->) r) = r
    ask       = id
    local f m = m . f

-- ---------------------------------------------------------------------------
-- Instances for other mtl transformers

instance (MonadReader m) => MonadReader (ContT r m) where
    type EnvType (ContT r m) = EnvType m
    ask       = lift ask
    local f m = ContT $ \c -> do
        r <- ask
        local f (runContT m (local (const r) . c))

instance (MonadReader m) => MonadReader (ExceptT e m) where
    type EnvType (ExceptT e m) = EnvType m
    ask       = lift ask
    local f m = ExceptT $ local f (runExceptT m)

instance (Monoid w, Monad m) => MonadReader (RWST r w s m) where
    type EnvType (RWST r w s m) = r
    ask       = RWST $ \r s -> return (r, s, mempty)
    local f m = RWST $ \r s -> runRWST m (f r) s

instance (Monoid w, Monad m) => MonadReader (Strict.RWST r w s m) where
    type EnvType (Strict.RWST r w s m) = r
    ask       = Strict.RWST $ \r s -> return (r, s, mempty)
    local f m = Strict.RWST $ \r s -> Strict.runRWST m (f r) s

instance (MonadReader m) => MonadReader (StateT s m) where
    type EnvType (StateT s m) = EnvType m
    ask       = lift ask
    local f m = StateT $ \s -> local f (runStateT m s)

instance (MonadReader m) => MonadReader (Strict.StateT s m) where
    type EnvType (Strict.StateT s m) = EnvType m
    ask       = lift ask
    local f m = Strict.StateT $ \s -> local f (Strict.runStateT m s)

instance (Monoid w, MonadReader m) => MonadReader (WriterT w m) where
    type EnvType (WriterT w m) = EnvType m
    ask       = lift ask
    local f m = WriterT $ local f (runWriterT m)

instance (Monoid w, MonadReader m) => MonadReader (Strict.WriterT w m) where
    type EnvType (Strict.WriterT w m) = EnvType m
    ask       = lift ask
    local f m = Strict.WriterT $ local f (Strict.runWriterT m)
