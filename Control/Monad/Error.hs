{-# LANGUAGE UndecidableInstances #-}
-- Needed for the same reasons as in Reader, State etc

{- |
Module      :  Control.Monad.Error
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
module Control.Monad.Error (
    module Control.Monad.Error.Class,
    ErrorT(..),
    mapErrorT,
    module Control.Monad,
    module Control.Monad.Fix,
    module Control.Monad.Trans,
    -- * Example 1: Custom Error Data Type
    -- $customErrorExample

    -- * Example 2: Using ErrorT Monad Transformer
    -- $ErrorTExample
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.Cont.Class
import Control.Monad.Error.Class
import Control.Monad.Fix
import Control.Monad.RWS.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans
import Control.Monad.Trans.Error
import Control.Monad.Writer.Class

import Control.Monad.Instances ()
import Data.Monoid
import System.IO

type instance ErrorType IO = IOError

instance MonadError IO where
    throwError = ioError
    catchError = catch

-- ---------------------------------------------------------------------------
-- Our parameterizable error monad

type instance ErrorType (Either e) = e

instance (Error e) => MonadError (Either e) where
    throwError             = Left
    Left  l `catchError` h = h l
    Right r `catchError` _ = Right r

type instance ErrorType (ErrorT e m) = e

instance (Monad m, Error e) => MonadError (ErrorT e m) where
    throwError l     = ErrorT $ return (Left l)
    m `catchError` h = ErrorT $ do
        a <- runErrorT m
        case a of
            Left  l -> runErrorT (h l)
            Right r -> return (Right r)

-- ---------------------------------------------------------------------------
-- Instances for other mtl transformers

instance (Error e, MonadCont m) => MonadCont (ErrorT e m) where
    callCC f = ErrorT $
        callCC $ \c ->
        runErrorT (f (\a -> ErrorT $ c (Right a)))

instance (Error e, Monoid (WriterType m), MonadRWS m) => MonadRWS (ErrorT e m)

type instance EnvType (ErrorT e m) = EnvType m

instance (Error e, MonadReader m) => MonadReader (ErrorT e m) where
    ask       = lift ask
    local f m = ErrorT $ local f (runErrorT m)

type instance StateType (ErrorT e m) = StateType m

instance (Error e, MonadState m) => MonadState (ErrorT e m) where
    get = lift get
    put = lift . put

type instance WriterType (ErrorT e m) = WriterType m

instance (Error e, MonadWriter m) => MonadWriter (ErrorT e m) where
    tell     = lift . tell
    listen m = ErrorT $ do
        (a, w) <- listen (runErrorT m)
        case a of
            Left  l -> return $ Left  l
            Right r -> return $ Right (r, w)
    pass   m = ErrorT $ pass $ do
        a <- runErrorT m
        case a of
            Left  l      -> return (Left  l, id)
            Right (r, f) -> return (Right r, f)

{- $customErrorExample
Here is an example that demonstrates the use of a custom 'Error' data type with
the 'throwError' and 'catchError' exception mechanism from 'MonadError'.
The example throws an exception if the user enters an empty string
or a string longer than 5 characters. Otherwise it prints length of the string.

>-- This is the type to represent length calculation error.
>data LengthError = EmptyString  -- Entered string was empty.
>          | StringTooLong Int   -- A string is longer than 5 characters.
>                                -- Records a length of the string.
>          | OtherError String   -- Other error, stores the problem description.
>
>-- We make LengthError an instance of the Error class
>-- to be able to throw it as an exception.
>instance Error LengthError where
>  noMsg    = OtherError "A String Error!"
>  strMsg s = OtherError s
>
>-- Converts LengthError to a readable message.
>instance Show LengthError where
>  show EmptyString = "The string was empty!"
>  show (StringTooLong len) =
>      "The length of the string (" ++ (show len) ++ ") is bigger than 5!"
>  show (OtherError msg) = msg
>
>-- For our monad type constructor, we use Either LengthError
>-- which represents failure using Left LengthError
>-- or a successful result of type a using Right a.
>type LengthMonad = Either LengthError
>
>main = do
>  putStrLn "Please enter a string:"
>  s <- getLine
>  reportResult (calculateLength s)
>
>-- Wraps length calculation to catch the errors.
>-- Returns either length of the string or an error.
>calculateLength :: String -> LengthMonad Int
>calculateLength s = (calculateLengthOrFail s) `catchError` Left
>
>-- Attempts to calculate length and throws an error if the provided string is
>-- empty or longer than 5 characters.
>-- The processing is done in Either monad.
>calculateLengthOrFail :: String -> LengthMonad Int
>calculateLengthOrFail [] = throwError EmptyString
>calculateLengthOrFail s | len > 5 = throwError (StringTooLong len)
>                        | otherwise = return len
>  where len = length s
>
>-- Prints result of the string length calculation.
>reportResult :: LengthMonad Int -> IO ()
>reportResult (Right len) = putStrLn ("The length of the string is " ++ (show len))
>reportResult (Left e) = putStrLn ("Length calculation failed with error: " ++ (show e))
-}

{- $ErrorTExample
@'ErrorT'@ monad transformer can be used to add error handling to another monad.
Here is an example how to combine it with an @IO@ monad:

>import Control.Monad.Error
>
>-- An IO monad which can return String failure.
>-- It is convenient to define the monad type of the combined monad,
>-- especially if we combine more monad transformers.
>type LengthMonad = ErrorT String IO
>
>main = do
>  -- runErrorT removes the ErrorT wrapper
>  r <- runErrorT calculateLength
>  reportResult r
>
>-- Asks user for a non-empty string and returns its length.
>-- Throws an error if user enters an empty string.
>calculateLength :: LengthMonad Int
>calculateLength = do
>  -- all the IO operations have to be lifted to the IO monad in the monad stack
>  liftIO $ putStrLn "Please enter a non-empty string: "
>  s <- liftIO getLine
>  if null s
>    then throwError "The string was empty!"
>    else return $ length s
>
>-- Prints result of the string length calculation.
>reportResult :: Either String Int -> IO ()
>reportResult (Right len) = putStrLn ("The length of the string is " ++ (show len))
>reportResult (Left e) = putStrLn ("Length calculation failed with error: " ++ (show e))
-}

