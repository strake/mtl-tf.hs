{- |
Module      :  Control.Monad.Error.Class
Copyright   :  (c) Michael Weber <michael.weber@post.rwth-aachen.de> 2001,
               (c) Jeff Newbern 2003-2006,
               (c) Andriy Palamarchuk 2006
License     :  BSD-style (see the file libraries/base/LICENSE)

Maintainer  :  libraries@haskell.org
Stability   :  experimental
Portability :  non-portable (multi-parameter type classes)

[Computation type:] Computations which may fail or throw exceptions.

[Binding strategy:] Failure records information about the cause\/location
of the failure. Failure values bypass the bound function,
other values are used as inputs to the bound function.

[Useful for:] Building computations from sequences of functions that may fail
or using exception handling to structure error handling.

[Zero and plus:] Zero is represented by an empty error and the plus operation
executes its second argument if the first fails.

[Example type:] @'Data.Either' String a@

The Error monad (also called the Exception monad).
-}

{-
  Rendered by Michael Weber <mailto:michael.weber@post.rwth-aachen.de>,
  inspired by the Haskell Monad Template Library from
    Andy Gill (<http://www.cse.ogi.edu/~andy/>)
-}
module Control.Monad.Error.Class (MonadError(..)) where

import Control.Exception
import Control.Monad.Trans.All
import qualified Control.Monad.Trans.All.Strict as Strict
import Control.Monad.Trans.Class
import Control.Monad.Trans.Identity (IdentityT (..))
import Control.Monad.Trans.Maybe (MaybeT (..))

{- |
The strategy of combining computations that can throw exceptions
by bypassing bound functions
from the point an exception is thrown to the point that it is handled.

Is parameterized over the type of error information and
the monad type constructor.
It is common to use @'Data.Either' String@ as the monad type constructor
for an error monad in which error descriptions take the form of strings.
In that case and many other common cases the resulting monad is already defined
as an instance of the 'MonadError' class.
You can also define your own error type and\/or use a monad type constructor
other than @'Data.Either' String@ or @'Data.Either' IOError@.
In these cases you will have to explicitly define instances of the 'Error'
and\/or 'MonadError' classes.
-}
class (Monad m) => MonadError m where
    type ErrorType m

    -- | Is used within a monadic computation to begin exception processing.
    throwError :: ErrorType m -> m a

    {- |
    A handler function to handle previous errors and return to normal execution.
    A common idiom is:

    > do { action1; action2; action3 } `catchError` handler

    where the @action@ functions can call 'throwError'.
    Note that @handler@ and the do-block must have the same return type.
    -}
    catchError :: m a -> (ErrorType m -> m a) -> m a

instance MonadError IO where
    type ErrorType IO = IOError
    throwError = ioError
    catchError = catch

-- ---------------------------------------------------------------------------
-- Our parameterizable error monad

instance MonadError (Either e) where
    type ErrorType (Either e) = e
    throwError             = Left
    Left  l `catchError` h = h l
    Right r `catchError` _ = Right r

instance (Monad m) => MonadError (ExceptT e m) where
    type ErrorType (ExceptT e m) = e
    throwError l     = ExceptT $ return (Left l)
    m `catchError` h = ExceptT $ runExceptT m >>= \ case
        Left  l -> runExceptT (h l)
        Right r -> return (Right r)

instance (MonadError m) => MonadError (IdentityT m) where
    type ErrorType (IdentityT m) = ErrorType m
    throwError = lift . throwError
    catchError (IdentityT x) f = IdentityT $ catchError x (runIdentityT . f)

instance (MonadError m) => MonadError (MaybeT m) where
    type ErrorType (MaybeT m) = ErrorType m
    throwError = lift . throwError
    catchError (MaybeT x) f = MaybeT $ catchError x (runMaybeT . f)

instance (MonadError m) => MonadError (ReaderT r m) where
    type ErrorType (ReaderT r m) = ErrorType m
    throwError       = lift . throwError
    m `catchError` h = ReaderT $ \r -> runReaderT m r
        `catchError` \e -> runReaderT (h e) r

instance (Monoid w, MonadError m) => MonadError (RWST r w s m) where
    type ErrorType (RWST r w s m) = ErrorType m
    throwError       = lift . throwError
    m `catchError` h = RWST $ \r s -> runRWST m r s
        `catchError` \e -> runRWST (h e) r s

instance (Monoid w, MonadError m) => MonadError (Strict.RWST r w s m) where
    type ErrorType (Strict.RWST r w s m) = ErrorType m
    throwError       = lift . throwError
    m `catchError` h = Strict.RWST $ \r s -> Strict.runRWST m r s
        `catchError` \e -> Strict.runRWST (h e) r s

instance (MonadError m) => MonadError (StateT s m) where
    type ErrorType (StateT s m) = ErrorType m
    throwError       = lift . throwError
    m `catchError` h = StateT $ \s -> runStateT m s
        `catchError` \e -> runStateT (h e) s

instance (MonadError m) => MonadError (Strict.StateT s m) where
    type ErrorType (Strict.StateT s m) = ErrorType m
    throwError       = lift . throwError
    m `catchError` h = Strict.StateT $ \s -> Strict.runStateT m s
        `catchError` \e -> Strict.runStateT (h e) s

instance (Monoid w, MonadError m) => MonadError (WriterT w m) where
    type ErrorType (WriterT w m) = ErrorType m
    throwError       = lift . throwError
    m `catchError` h = WriterT $ runWriterT m
        `catchError` \e -> runWriterT (h e)

instance (Monoid w, MonadError m) => MonadError (Strict.WriterT w m) where
    type ErrorType (Strict.WriterT w m) = ErrorType m
    throwError       = lift . throwError
    m `catchError` h = Strict.WriterT $ Strict.runWriterT m
        `catchError` \e -> Strict.runWriterT (h e)
