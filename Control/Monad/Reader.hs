{-# LANGUAGE UndecidableInstances #-}
{- |
Module      :  Control.Monad.Reader
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

module Control.Monad.Reader (
    module Control.Monad.Reader.Class,
    Reader,
    mapReader,
    withReader,
    ReaderT(..),
    mapReaderT,
    withReaderT,
    module Control.Monad,
    module Control.Monad.Fix,
    module Control.Monad.Trans,
    -- * Example 1: Simple Reader Usage
    -- $simpleReaderExample

    -- * Example 2: Modifying Reader Content With @local@
    -- $localExample

    -- * Example 3: @ReaderT@ Monad Transformer
    -- $ReaderTExample
    ) where

import Control.Monad
import Control.Monad.Cont.Class
import Control.Monad.Error.Class
import Control.Monad.Fix
import Control.Monad.Instances ()
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Writer.Class

type instance EnvType (ReaderT r m) = r

instance (Monad m) => MonadReader (ReaderT r m) where
    ask       = ReaderT return
    local f m = ReaderT $ \r -> runReaderT m (f r)

-- ----------------------------------------------------------------------------
-- The partially applied function type is a simple reader monad

type instance EnvType ((->) r) = r

instance MonadReader ((->) r) where
    ask       = id
    local f m = m . f

-- ---------------------------------------------------------------------------
-- Instances for other mtl transformers

instance (MonadCont m) => MonadCont (ReaderT r m) where
    callCC f = ReaderT $ \r ->
        callCC $ \c ->
        runReaderT (f (\a -> ReaderT $ \_ -> c a)) r

type instance ErrorType (ReaderT r m) = ErrorType m

instance (MonadError m) => MonadError (ReaderT r m) where
    throwError       = lift . throwError
    m `catchError` h = ReaderT $ \r -> runReaderT m r
        `catchError` \e -> runReaderT (h e) r

type instance StateType (ReaderT r m) = StateType m 

-- Needs UndecidableInstances
instance (MonadState m) => MonadState (ReaderT r m) where
    get = lift get
    put = lift . put

type instance WriterType (ReaderT r m) = WriterType m

-- This instance needs UndecidableInstances, because
-- it does not satisfy the coverage condition
instance (MonadWriter m) => MonadWriter (ReaderT r m) where
    tell     = lift . tell
    listen m = ReaderT $ \w -> listen (runReaderT m w)
    pass   m = ReaderT $ \w -> pass   (runReaderT m w)

{- $simpleReaderExample

In this example the @Reader@ monad provides access to variable bindings.
Bindings are a 'Map' of integer variables.
The variable @count@ contains number of variables in the bindings.
You can see how to run a Reader monad and retrieve data from it
with 'runReader', how to access the Reader data with 'ask' and 'asks'.

> type Bindings = Map String Int;
>
>-- Returns True if the "count" variable contains correct bindings size.
>isCountCorrect :: Bindings -> Bool
>isCountCorrect bindings = runReader calc_isCountCorrect bindings
>
>-- The Reader monad, which implements this complicated check.
>calc_isCountCorrect :: Reader Bindings Bool
>calc_isCountCorrect = do
>    count <- asks (lookupVar "count")
>    bindings <- ask
>    return (count == (Map.size bindings))
>
>-- The selector function to  use with 'asks'.
>-- Returns value of the variable with specified name.
>lookupVar :: String -> Bindings -> Int
>lookupVar name bindings = fromJust (Map.lookup name bindings)
>
>sampleBindings = Map.fromList [("count",3), ("1",1), ("b",2)]
>
>main = do
>    putStr $ "Count is correct for bindings " ++ (show sampleBindings) ++ ": ";
>    putStrLn $ show (isCountCorrect sampleBindings);
-}

{- $localExample

Shows how to modify Reader content with 'local'.

>calculateContentLen :: Reader String Int
>calculateContentLen = do
>    content <- ask
>    return (length content);
>
>-- Calls calculateContentLen after adding a prefix to the Reader content.
>calculateModifiedContentLen :: Reader String Int
>calculateModifiedContentLen = local ("Prefix " ++) calculateContentLen
>
>main = do
>    let s = "12345";
>    let modifiedLen = runReader calculateModifiedContentLen s
>    let len = runReader calculateContentLen s
>    putStrLn $ "Modified 's' length: " ++ (show modifiedLen)
>    putStrLn $ "Original 's' length: " ++ (show len)
-}

{- $ReaderTExample

Now you are thinking: 'Wow, what a great monad! I wish I could use
Reader functionality in MyFavoriteComplexMonad!'. Don't worry.
This can be easy done with the 'ReaderT' monad transformer.
This example shows how to combine @ReaderT@ with the IO monad.

>-- The Reader/IO combined monad, where Reader stores a string.
>printReaderContent :: ReaderT String IO ()
>printReaderContent = do
>    content <- ask
>    liftIO $ putStrLn ("The Reader Content: " ++ content)
>
>main = do
>    runReaderT printReaderContent "Some Content"
-}
