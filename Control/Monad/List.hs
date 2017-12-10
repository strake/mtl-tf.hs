{-# LANGUAGE UndecidableInstances #-}
-- Needed for the same reasons as in Reader, State etc

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.List
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (multi-parameter type classes)
--
-- The List monad.
--
-----------------------------------------------------------------------------

module Control.Monad.List (
    ListT(..),
    mapListT,
    module Control.Monad,
    module Control.Monad.Trans,
  ) where

import Control.Monad
import Control.Monad.Cont.Class
import Control.Monad.Error.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans
import Control.Monad.Trans.List

-- ---------------------------------------------------------------------------
-- Instances for other mtl transformers

instance (MonadCont m) => MonadCont (ListT m) where
    callCC f = ListT $
        callCC $ \c ->
        runListT (f (\a -> ListT $ c [a]))

instance (MonadError m) => MonadError (ListT m) where
    type ErrorType (ListT m) = ErrorType m
    throwError       = lift . throwError
    m `catchError` h = ListT $ runListT m
        `catchError` \e -> runListT (h e)

instance (MonadReader m) => MonadReader (ListT m) where
    type EnvType (ListT m) = EnvType m
    ask       = lift ask
    local f m = ListT $ local f (runListT m)

instance (MonadState m) => MonadState (ListT m) where
    type StateType (ListT m) = StateType m
    get = lift get
    put = lift . put

