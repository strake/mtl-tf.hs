-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.RWS.Class
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (multi-param classes, functional dependencies)
--
-- Declaration of the MonadRWS class.
--
--      Inspired by the paper
--      /Functional Programming with Overloading and
--          Higher-Order Polymorphism/,
--        Mark P Jones (<http://www.cse.ogi.edu/~mpj/>)
--          Advanced School of Functional Programming, 1995.
-----------------------------------------------------------------------------

module Control.Monad.RWS.Class (
    MonadRWS,
    module Control.Monad.Reader.Class,
    module Control.Monad.State.Class,
    module Control.Monad.Writer.Class,
  ) where

import Control.Monad.Trans.All
import qualified Control.Monad.Trans.All.Strict as Strict
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Writer.Class

class (Monoid (WritType m), MonadReader m, MonadWriter m, MonadState m)
   => MonadRWS m

instance (Error e, Monoid (WritType m), MonadRWS m) => MonadRWS (ErrorT e m)

instance (Monoid w, Monad m) => MonadRWS (RWST r w s m)

instance (Monoid w, Monad m) => MonadRWS (Strict.RWST r w s m)
