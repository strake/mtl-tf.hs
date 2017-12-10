{-# LANGUAGE UndecidableInstances #-}
-- Search for UndecidableInstances to see why this is needed

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Writer.Strict
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (multi-param classes, functional dependencies)
--
-- Strict writer monads.
--
--      Inspired by the paper
--      /Functional Programming with Overloading and
--          Higher-Order Polymorphism/,
--        Mark P Jones (<http://web.cecs.pdx.edu/~mpj/pubs/springschool.html>)
--          Advanced School of Functional Programming, 1995.
-----------------------------------------------------------------------------

module Control.Monad.Writer.Strict (
    module Control.Monad.Writer.Class,
    Writer,
    execWriter,
    mapWriter,
    WriterT(..),
    execWriterT,
    mapWriterT,
    module Control.Monad,
    module Control.Monad.Fix,
    module Control.Monad.Trans,
    module Data.Monoid,
  ) where

import Control.Monad
import Control.Monad.Cont.Class
import Control.Monad.Error.Class
import Control.Monad.Fix
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans
import Control.Monad.Trans.Writer.Strict
import Control.Monad.Writer.Class
import Data.Monoid

instance (Monoid w, Monad m) => MonadWriter (WriterT w m) where
    type WritType (WriterT w m) = w
    tell   w = WriterT $ return ((), w)
    listen m = WriterT $ do
        (a, w) <- runWriterT m
        return ((a, w), w)
    pass   m = WriterT $ do
        ((a, f), w) <- runWriterT m
        return (a, f w)

-- ---------------------------------------------------------------------------
-- Instances for other mtl transformers

instance (Monoid w, MonadCont m) => MonadCont (WriterT w m) where
    callCC f = WriterT $
        callCC $ \c ->
        runWriterT (f (\a -> WriterT $ c (a, mempty)))

instance (Monoid w, MonadError m) => MonadError (WriterT w m) where
    type ErrorType (WriterT w m) = ErrorType m
    throwError       = lift . throwError
    m `catchError` h = WriterT $ runWriterT m
        `catchError` \e -> runWriterT (h e)

-- This instance needs UndecidableInstances, because
-- it does not satisfy the coverage condition
instance (Monoid w, MonadReader m) => MonadReader (WriterT w m) where
    type EnvType (WriterT w m) = EnvType m
    ask       = lift ask
    local f m = WriterT $ local f (runWriterT m)

-- Needs UndecidableInstances
instance (Monoid w, MonadState m) => MonadState (WriterT w m) where
    type StateType (WriterT w m) = StateType m
    get = lift get
    put = lift . put
