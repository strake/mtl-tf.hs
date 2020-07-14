-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Writer.Class
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (multi-param classes, functional dependencies)
--
-- The MonadWriter class.
--
--      Inspired by the paper
--      /Functional Programming with Overloading and
--          Higher-Order Polymorphism/,
--        Mark P Jones (<http://web.cecs.pdx.edu/~mpj/pubs/springschool.html>)
--          Advanced School of Functional Programming, 1995.
-----------------------------------------------------------------------------

module Control.Monad.Writer.Class (
    MonadWriter(..),
    listens,
    censor,
  ) where

import Control.Monad.Trans.All
import qualified Control.Monad.Trans.All.Strict as Strict
import Control.Monad.Trans.Class
import Control.Monad.Trans.Identity (IdentityT (..))
import Control.Monad.Trans.Maybe (MaybeT (..))

-- ---------------------------------------------------------------------------
-- MonadWriter class
--
-- tell is like tell on the MUD's it shouts to monad
-- what you want to be heard. The monad carries this 'packet'
-- upwards, merging it if needed (hence the Monoid requirement)}
--
-- listen listens to a monad acting, and returns what the monad "said".
--
-- pass lets you provide a writer transformer which changes internals of
-- the written object.

class (Monoid (WritType m), Monad m) => MonadWriter m where
    type WritType m
    tell   :: WritType m -> m ()
    listen :: m a -> m (a, WritType m)
    pass   :: m (a, WritType m -> WritType m) -> m a

listens :: (MonadWriter m) => (WritType m -> b) -> m a -> m (a, b)
listens f m = do
    ~(a, w) <- listen m
    return (a, f w)

censor :: (MonadWriter m) => (WritType m -> WritType m) -> m a -> m a
censor f m = pass $ do
    a <- m
    return (a, f)

instance (MonadWriter m) => MonadWriter (IdentityT m) where
    type WritType (IdentityT m) = WritType m
    tell = lift . tell
    listen (IdentityT m) = IdentityT $ listen m
    pass   (IdentityT m) = IdentityT $ pass m

instance (MonadWriter m) => MonadWriter (MaybeT m) where
    type WritType (MaybeT m) = WritType m
    tell = lift . tell
    listen (MaybeT m) = MaybeT $ flip fmap (listen m) $ \ (a, w) -> flip (,) w <$> a
    pass   (MaybeT m) = MaybeT $ pass $ (\ case Nothing -> (Nothing, id); Just (r, f) -> (Just r, f)) <$> m

instance (MonadWriter m) => MonadWriter (ExceptT e m) where
    type WritType (ExceptT e m) = WritType m
    tell     = lift . tell
    listen (ExceptT m) = ExceptT $ flip fmap (listen m) $ \ (a, w) -> flip (,) w <$> a
    pass   (ExceptT m) = ExceptT $ pass $ (\ case Left  l      -> (Left  l, id); Right (r, f) -> (Right r, f)) <$> m

instance (MonadWriter m) => MonadWriter (ReaderT r m) where
    type WritType (ReaderT r m) = WritType m
    tell     = lift . tell
    listen m = ReaderT $ \w -> listen (runReaderT m w)
    pass   m = ReaderT $ \w -> pass   (runReaderT m w)

instance (Monoid w, Monad m) => MonadWriter (RWST r w s m) where
    type WritType (RWST r w s m) = w
    tell   w = RWST $ \_ s -> return ((),s,w)
    listen m = RWST $ \r s -> do
        ~(a, s', w) <- runRWST m r s
        return ((a, w), s', w)
    pass   m = RWST $ \r s -> do
        ~((a, f), s', w) <- runRWST m r s
        return (a, s', f w)

instance (Monoid w, Monad m) => MonadWriter (Strict.RWST r w s m) where
    type WritType (Strict.RWST r w s m) = w
    tell   w = Strict.RWST $ \_ s -> return ((),s,w)
    listen m = Strict.RWST $ \r s -> do
        (a, s', w) <- Strict.runRWST m r s
        return ((a, w), s', w)
    pass   m = Strict.RWST $ \r s -> do
        ((a, f), s', w) <- Strict.runRWST m r s
        return (a, s', f w)

instance (MonadWriter m) => MonadWriter (StateT s m) where
    type WritType (StateT s m) = WritType m
    tell     = lift . tell
    listen m = StateT $ \s -> do
        ~((a, s'), w) <- listen (runStateT m s)
        return ((a, w), s')
    pass   m = StateT $ \s -> pass $ do
        ~((a, f), s') <- runStateT m s
        return ((a, s'), f)

instance (MonadWriter m) => MonadWriter (Strict.StateT s m) where
    type WritType (Strict.StateT s m) = WritType m
    tell     = lift . tell
    listen m = Strict.StateT $ \s -> do
        ((a, s'), w) <- listen (Strict.runStateT m s)
        return ((a, w), s')
    pass   m = Strict.StateT $ \s -> pass $ do
        ((a, f), s') <- Strict.runStateT m s
        return ((a, s'), f)

instance (Monoid w, Monad m) => MonadWriter (WriterT w m) where
    type WritType (WriterT w m) = w
    tell   w = WriterT $ return ((), w)
    listen m = WriterT $ do
        ~(a, w) <- runWriterT m
        return ((a, w), w)
    pass   m = WriterT $ do
        ~((a, f), w) <- runWriterT m
        return (a, f w)

instance (Monoid w, Monad m) => MonadWriter (Strict.WriterT w m) where
    type WritType (Strict.WriterT w m) = w
    tell   w = Strict.WriterT $ return ((), w)
    listen m = Strict.WriterT $ do
        (a, w) <- Strict.runWriterT m
        return ((a, w), w)
    pass   m = Strict.WriterT $ do
        ((a, f), w) <- Strict.runWriterT m
        return (a, f w)
