{-# LANGUAGE UndecidableInstances #-}
-- Search for UndecidableInstances to see why this is needed

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Writer.Lazy
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (multi-param classes, functional dependencies)
--
-- Lazy writer monads.
--
--      Inspired by the paper
--      /Functional Programming with Overloading and
--          Higher-Order Polymorphism/,
--        Mark P Jones (<http://web.cecs.pdx.edu/~mpj/pubs/springschool.html>)
--          Advanced School of Functional Programming, 1995.
-----------------------------------------------------------------------------

module Control.Monad.Writer.Lazy (
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
import Control.Monad.Trans.Writer.Lazy
import Control.Monad.Writer.Class
import Data.Monoid

type instance WriterType (WriterT w m) = w

instance (Monoid w, Monad m) => MonadWriter (WriterT w m) where
    tell   w = WriterT $ return ((), w)
    listen m = WriterT $ do
        ~(a, w) <- runWriterT m
        return ((a, w), w)
    pass   m = WriterT $ do
        ~((a, f), w) <- runWriterT m
        return (a, f w)

-- ---------------------------------------------------------------------------
-- Instances for other mtl transformers

instance (Monoid w, MonadCont m) => MonadCont (WriterT w m) where
    callCC f = WriterT $
        callCC $ \c ->
        runWriterT (f (\a -> WriterT $ c (a, mempty)))

type instance ErrorType (WriterT w m) = ErrorType m

instance (Monoid w, MonadError m) => MonadError (WriterT w m) where
    throwError       = lift . throwError
    m `catchError` h = WriterT $ runWriterT m
        `catchError` \e -> runWriterT (h e)

type instance EnvType (WriterT w m) = EnvType m

-- This instance needs UndecidableInstances, because
-- it does not satisfy the coverage condition
instance (Monoid w, MonadReader m) => MonadReader (WriterT w m) where
    ask       = lift ask
    local f m = WriterT $ local f (runWriterT m)

type instance StateType (WriterT w m) = StateType m

-- Needs UndecidableInstances
instance (Monoid w, MonadState m) => MonadState (WriterT w m) where
    get = lift get
    put = lift . put

