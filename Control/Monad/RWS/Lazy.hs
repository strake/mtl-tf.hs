{-# LANGUAGE UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.RWS.Lazy
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (multi-param classes, functional dependencies)
--
-- Lazy RWS monad.
--
--      Inspired by the paper
--      /Functional Programming with Overloading and
--          Higher-Order Polymorphism/,
--        Mark P Jones (<http://www.cse.ogi.edu/~mpj/>)
--          Advanced School of Functional Programming, 1995.
-----------------------------------------------------------------------------

module Control.Monad.RWS.Lazy (
    RWS,
    evalRWS,
    execRWS,
    mapRWS,
    withRWS,
    RWST(..),
    evalRWST,
    execRWST,
    mapRWST,
    withRWST,
    module Control.Monad.RWS.Class,
    module Control.Monad,
    module Control.Monad.Fix,
    module Control.Monad.Trans,
    module Data.Monoid,
  ) where

import Control.Monad
import Control.Monad.Cont.Class
import Control.Monad.Error.Class
import Control.Monad.Fix
import Control.Monad.RWS.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans
import Control.Monad.Trans.RWS.Lazy
import Control.Monad.Writer.Class
import Data.Monoid

type instance EnvType (RWST r w s m) = r

instance (Monoid w, Monad m) => MonadReader (RWST r w s m) where
    ask       = RWST $ \r s -> return (r, s, mempty)
    local f m = RWST $ \r s -> runRWST m (f r) s

type instance WriterType (RWST r w s m) = w

instance (Monoid w, Monad m) => MonadWriter (RWST r w s m) where
    tell   w = RWST $ \_ s -> return ((),s,w)
    listen m = RWST $ \r s -> do
        ~(a, s', w) <- runRWST m r s
        return ((a, w), s', w)
    pass   m = RWST $ \r s -> do
        ~((a, f), s', w) <- runRWST m r s
        return (a, s', f w)

type instance StateType (RWST r w s m) = s

instance (Monoid w, Monad m) => MonadState (RWST r w s m) where
    get   = RWST $ \_ s -> return (s, s, mempty)
    put s = RWST $ \_ _ -> return ((), s, mempty)

instance (Monoid w, Monad m) => MonadRWS (RWST r w s m)

-- ---------------------------------------------------------------------------
-- Instances for other mtl transformers

instance (Monoid w, MonadCont m) => MonadCont (RWST r w s m) where
    callCC f = RWST $ \r s ->
        callCC $ \c ->
        runRWST (f (\a -> RWST $ \_ s' -> c (a, s', mempty))) r s

type instance ErrorType (RWST r w s m) = ErrorType m

instance (Monoid w, MonadError m) => MonadError (RWST r w s m) where
    throwError       = lift . throwError
    m `catchError` h = RWST $ \r s -> runRWST m r s
        `catchError` \e -> runRWST (h e) r s

