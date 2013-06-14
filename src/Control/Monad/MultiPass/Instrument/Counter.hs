-- Copyright 2013 Kevin Backhouse.

{-# OPTIONS_GHC -XKindSignatures #-}

{-|
The 'Counter' instrument is used to generate an increasing
sequence of integers. It is particularly useful when the program
uses parallelism, because the 'Counter' instrument creates the
illusion of a single-threaded global counter. The first pass
counts how many unique integers each thread needs so that the
integers can be generated without the use of locks during the
second pass.
-}

module Control.Monad.MultiPass.Instrument.Counter
  ( Counter
  , peek, addk, incr, preIncr, postIncr
  )
where

import Control.Monad ( void )
import Control.Monad.MultiPass
import Control.Monad.MultiPass.ThreadContext.CounterTC

-- | Abstract datatype for the instrument.
data Counter i r w (p1 :: * -> *) p2 tc
  = Counter
      { peekInternal :: !(MultiPass r w tc (p2 i))
      , addkInternal :: !(p1 i -> MultiPass r w tc ())
      }

-- | Get the current value of the counter.
peek
  :: (Num i, Monad p1, Monad p2)
  => Counter i r w p1 p2 tc
  -> MultiPass r w tc (p2 i)
peek =
  peekInternal

-- | Add @k@ to the counter.
addk
  :: (Num i, Monad p1, Monad p2)
  => Counter i r w p1 p2 tc        -- ^ counter
  -> p1 i                          -- ^ k
  -> MultiPass r w tc ()
addk =
  addkInternal

-- | Increment the counter.
incr
  :: (Num i, Monad p1, Monad p2)
  => Counter i r w p1 p2 tc
  -> MultiPass r w tc ()
incr c = addk c (return 1)

-- | Read and pre-increment the counter. For example, if the current
-- value is 17 then 'preIncr' updates the value of the counter to 18
-- and returns 18.
preIncr
  :: (Num i, Monad p1, Monad p2)
  => Counter i r w p1 p2 tc
  -> MultiPass r w tc (p2 i)
preIncr c =
  do incr c
     peek c

-- | Read and post-increment the counter. For example, if the current
-- value is 17 then 'postIncr' updates the value of the counter to 18
-- and returns 17.
postIncr
  :: (Num i, Monad p1, Monad p2)
  => Counter i r w p1 p2 tc
  -> MultiPass r w tc (p2 i)
postIncr c =
  do v <- peek c
     incr c
     return v

instance Instrument tc () () (Counter i r w Off Off tc) where
  createInstrument _ _ () =
    wrapInstrument $ Counter
      { peekInternal = return Off
      , addkInternal = \Off -> return ()
      }

-- Pass 1 of the Counter. This pass tracks the number of integers
-- that are requested in each thread. At the end of the first pass,
-- cumsum is used to assign disjoint ranges of integers to each
-- thread.
instance Num i =>
         Instrument tc (CounterTC1 i r) ()
                    (Counter i r w On Off tc) where
  createInstrument _ updateCtx () =
    wrapInstrument $ Counter
      { peekInternal = return Off

      , addkInternal = \(On k) ->
          void $ mkMultiPass $ updateCtx $ addkCounterTC1 k
      }

-- Pass 2 of the Counter. The array has one counter per thread.
instance Num i =>
         Instrument tc (CounterTC2 i r) ()
                    (Counter i r w On On tc) where
  createInstrument _ updateCtx () =
    wrapInstrument $ Counter
      { peekInternal =
          mkMultiPass $
          do counter <- updateCtx id
             return (On (counterVal2 counter))

      , addkInternal = \(On k) ->
          void $ mkMultiPass $ updateCtx $ addkCounterTC2 k
      }
