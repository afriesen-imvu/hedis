
module Database.Redis.Unsafe (
    Queued (..)
) where

import Database.Redis.Protocol (Reply)
import Data.Vector (Vector)

-- |A 'Queued' value represents the result of a command inside a transaction. It
--  is a proxy object for the /actual/ result, which will only be available
--  after returning from a 'multiExec' transaction.
--
--  'Queued' values are composable by utilizing the 'Functor', 'Applicative' or
--  'Monad' interfaces.
data Queued a = Queued (Vector Reply -> Either Reply a)

instance Functor Queued where
    fmap f (Queued g) = Queued (fmap f . g)

instance Applicative Queued where
    pure x                = Queued (const $ Right x)
    Queued f <*> Queued x = Queued $ \rs -> do
                                        f' <- f rs
                                        x' <- x rs
                                        return (f' x')

instance Monad Queued where
    return         = pure
    Queued x >>= f = Queued $ \rs -> do
                                x' <- x rs
                                let Queued f' = f x'
                                f' rs

