-- Copyright 2013 Kevin Backhouse.

-- | 'Control.Monad.MultiPass.ThreadContext.CounterTC' defines a
-- thread context which is used to generate a series of unique
-- consecutive numbers. It has two passes. The first pass,
-- 'CounterTC1', creates a log of the number of new values that need
-- to be generated in each thread. The second pass, 'CounterTC2', uses
-- the log to compute the correct starting value for each thread, so
-- that the threads appear to be incrementing a single global counter,
-- even though they are operating concurrently.

module Control.Monad.MultiPass.ThreadContext.CounterTC
  ( -- * First Pass
    CounterTC1
  , counterVal1, incrCounterTC1, addkCounterTC1
  , newCounterTC1

    -- * Second Pass
  , CounterTC2
  , counterVal2, incrCounterTC2, addkCounterTC2
  , newCounterTC2, resetCounterTC2
  )
where

import Control.Monad.State.Strict
import Control.Monad.ST2
import Control.Monad.MultiPass

data CounterLogSequential i r
  = CounterLogSequential !i !(ST2RArray r Int (CounterLogParallel i r))

newtype CounterLogParallel i r
  = CounterLogParallel (ST2RArray r Int (CounterLogSequential i r))

-- | 'CounterTC1' is used during the first pass. It builds up a log of
-- the parallel tasks that were spawned, which is used during the
-- second pass to generate a series of unique consecutive numbers.
data CounterTC1 i r
  = CounterTC1
      { -- Counter log for the current node. (Accumulates in reverse.)
        counterLog1 :: ![CounterLogParallel i r]

        -- | Get the current value of the counter.
      , counterVal1 :: !i
      }

instance Num i => ThreadContext r w (CounterTC1 i r) where
  splitThreadContext _ _ _ =
    return $ CounterTC1 [] 0

  mergeThreadContext m getSubNode node =
    do xs <- newST2Array_ (0,m-1)
       c <- flip execStateT 0 $
         sequence_
           [ do subnode0 <- lift $ getSubNode i
                c <- get
                let subnode1 = subnode0 { counterVal1 = c }
                put (c + counterVal1 subnode0)
                subnode2 <- lift $ mkCounterLogSequential subnode1
                lift $ writeST2Array xs i subnode2
           | i <- [0 .. m-1]
           ]
       let xs' = CounterLogParallel (mkST2RArray xs)
       return $ CounterTC1
         { counterLog1 = xs' : counterLog1 node
         , counterVal1 = c + counterVal1 node
         }

instance Num i =>
         NextThreadContext r w () gc (CounterTC1 i r) where
  nextThreadContext _ _ () _ =
    return newCounterTC1

instance Num i =>
         NextThreadContext r w (CounterTC1 i r) gc (CounterTC1 i r) where
  nextThreadContext _ _ _ _ =
    return newCounterTC1

-- | Create a new counter.
newCounterTC1 :: Num i => CounterTC1 i r
newCounterTC1 =
  CounterTC1 [] 0

-- | Increment the counter.
incrCounterTC1 :: Num i => CounterTC1 i r -> CounterTC1 i r
incrCounterTC1 = addkCounterTC1 1

-- | Add @k@ to the counter.
addkCounterTC1 :: Num i => i -> CounterTC1 i r -> CounterTC1 i r
addkCounterTC1 k (CounterTC1 h c) =
  CounterTC1 h (c+k)

-- The log has been accumulated as a list in reverse order. This
-- function reverses the list and converts it to a read-only array.
mkCounterLogSequential
  :: CounterTC1 i r
  -> ST2 r w (CounterLogSequential i r)
mkCounterLogSequential (CounterTC1 xs c) =
  let n = length xs in
  do xs' <- newST2Array_ (0,n-1)
     sequence_
       [ writeST2Array xs' (n-i) x
       | (x,i) <- zip xs [1 .. n]
       ]
     return (CounterLogSequential c (mkST2RArray xs'))

-- | 'CounterTC2' is used during the second pass. It uses the log
-- which was computed by 'CounterTC1' to generate a series of unique
-- consecutive numbers.
data CounterTC2 i r
  = CounterTC2
      { counterLog2 :: !(ST2RArray r Int (CounterLogParallel i r))

        -- Current index in the counter log.
      , counterIdx2 :: !Int

        -- | Get the current value of the counter.
      , counterVal2 :: !i
      }

-- | Increment the counter.
incrCounterTC2 :: Num i => CounterTC2 i r -> CounterTC2 i r
incrCounterTC2 = addkCounterTC2 1

-- | Add @k@ to the counter.
addkCounterTC2 :: Num i => i -> CounterTC2 i r -> CounterTC2 i r
addkCounterTC2 k node =
  node { counterVal2 = k + counterVal2 node }

instance Num i => ThreadContext r w (CounterTC2 i r) where
  splitThreadContext _ i node =
    do -- Read the current index of the log.
       CounterLogParallel ps <-
         readST2RArray (counterLog2 node) (counterIdx2 node)
       -- Get the log for thread i.
       CounterLogSequential k pss <- readST2RArray ps i
       return $ CounterTC2
         { counterLog2 = pss
         , counterIdx2 = 0
         , counterVal2 = k + counterVal2 node
         }

  mergeThreadContext m getSubNode node =
    do -- Get the new counter value from the last sub-node.
       lastSubNode <- getSubNode (m-1)
       return $ node
         { counterIdx2 = 1 + counterIdx2 node
         , counterVal2 = counterVal2 lastSubNode
         }

instance Num i =>
         NextThreadContext r w (CounterTC1 i r) gc (CounterTC2 i r) where
  nextThreadContext _ _ node _ =
    newCounterTC2 node

instance Num i =>
         NextThreadContext r w (CounterTC2 i r) gc (CounterTC1 i r) where
  nextThreadContext _ _ _ _ =
    return newCounterTC1

instance Num i =>
         NextThreadContext r w (CounterTC2 i r) gc (CounterTC2 i r) where
  nextThreadContext _ _ node _ =
    return (resetCounterTC2 node)

-- | Convert a 'CounterTC1' to a 'CounterTC2'.
newCounterTC2 :: Num i => CounterTC1 i r -> ST2 r w (CounterTC2 i r)
newCounterTC2 node =
  do CounterLogSequential _ pss <- mkCounterLogSequential node
     return $ CounterTC2
       { counterLog2 = pss
       , counterIdx2 = 0
       , counterVal2 = 0
       }

-- | Reset the counter to zero and rewind to the beginning of the log.
resetCounterTC2 :: Num i => CounterTC2 i r -> CounterTC2 i r
resetCounterTC2 node =
  node { counterIdx2 = 0, counterVal2 = 0 }
