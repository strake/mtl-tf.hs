{- |
Module      :  Control.Monad.Identity
Copyright   :  (c) Andy Gill 2001,
               (c) Oregon Graduate Institute of Science and Technology 2001,
               (c) Jeff Newbern 2003-2006,
               (c) Andriy Palamarchuk 2006
License     :  BSD-style (see the file libraries/base/LICENSE)

Maintainer  :  libraries@haskell.org
Stability   :  experimental
Portability :  portable

[Computation type:] Simple function application.

[Binding strategy:] The bound function is applied to the input value.
@'Identity' x >>= f == 'Identity' (f x)@

[Useful for:] Monads can be derived from monad transformers applied to the
'Identity' monad.

[Zero and plus:] None.

[Example type:] @'Identity' a@

The @Identity@ monad is a monad that does not embody any computational strategy.
It simply applies the bound function to its input without any modification.
Computationally, there is no reason to use the @Identity@ monad
instead of the much simpler act of simply applying functions to their arguments.
The purpose of the @Identity@ monad is its fundamental role in the theory
of monad transformers.
Any monad transformer applied to the @Identity@ monad yields a non-transformer
version of that monad.

  Inspired by the paper
  /Functional Programming with Overloading and
      Higher-Order Polymorphism/,
    Mark P Jones (<http://www.cse.ogi.edu/~mpj/>)
      Advanced School of Functional Programming, 1995.
-}

module Control.Monad.Identity (
    module Control.Monad,
    module Control.Monad.Fix,
    module Control.Monad.Trans.Identity,
    module Data.Functor.Identity,
   ) where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans.Identity
import Data.Functor.Identity
