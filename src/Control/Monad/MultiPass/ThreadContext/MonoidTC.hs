-- Copyright 2013 Kevin Backhouse.

-- | 'Control.Monad.MultiPass.ThreadContext.MonoidTC' defines a thread
-- context which is used to gather values from all the threads of the
-- program. The values to be gathered must be instances of the
-- 'Data.Monoid' class.

module Control.Monad.MultiPass.ThreadContext.MonoidTC ( MonoidTC(..) )
where

import Control.Monad.Writer.Strict
import Control.Monad.MultiPass

-- | MonoidTC is a thread context which uses the Monoid interface to
-- combine the values from multiple threads. Instances of the Monoid
-- class are expected to be associative, so the value computed by
-- MonoidTC is invariant under changes to the number of threads that
-- are spawned.
newtype MonoidTC a = MonoidTC { unwrapMonoidTC :: a }

instance Monoid a => Monoid (MonoidTC a) where
  mempty = MonoidTC mempty

  mappend (MonoidTC x) (MonoidTC y) =
    MonoidTC (mappend x y)

instance Monoid a => ThreadContext r w (MonoidTC a) where
  splitThreadContext _ _ _ =
    return $ mempty

  mergeThreadContext n f x =
    execWriterT $
    do tell x
       sequence_
         [ do y <- lift $ f i
              tell y
         | i <- [0 .. n-1]
         ]

instance Monoid a => NextThreadContext r w tc gc (MonoidTC a) where
  nextThreadContext _ _ _ _ =
    return mempty
