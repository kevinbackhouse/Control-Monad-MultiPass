-- Copyright 2013 Kevin Backhouse.

{-|
The 'CreateST2Array' instrument is stateless and provides a similar
interface to 'parallelMP'. The difference is that it produces the
new array in a specific pass.
-}

module Control.Monad.MultiPass.Instrument.CreateST2Array
  ( CreateST2Array
  , createST2Array, pmapST2ArrayMP
  )
where

import Control.Monad.ST2
import Control.Monad.MultiPass
import Data.Ix

-- | Abstract datatype for the instrument.
data CreateST2Array r w p1 tc
  = CreateST2Array
      { createInternal :: !(
          forall i a.
          (Ix i, Num i) =>
          NumThreads ->        -- Number of threads to spawn
          (i,i) ->             -- Element range
          (i -> MultiPass r w tc (p1 a)) ->
          MultiPass r w tc (p1 (ST2Array r w i a)))
      }

-- | Create a new array during pass @p1@, using the initialisation
-- function to initialise the elements. The initialisation is done in
-- parallel, using the specified number of threads.
createST2Array
  :: (Ix i, Num i, Monad p1)
  => CreateST2Array r w p1 tc        -- ^ 'CreateST2Array' instrument
  -> NumThreads                      -- ^ Number of threads to spawn
  -> (i,i)                           -- ^ Element range
  -> (i -> MultiPass r w tc (p1 a))  -- ^ Initialisation function
  -> MultiPass r w tc (p1 (ST2Array r w i a)) -- ^ New array
createST2Array =
  createInternal

instance Instrument tc () () (CreateST2Array r w Off tc) where
  createInstrument _ _ () =
    wrapInstrument $ CreateST2Array $ \m n f ->
    do parallelMP_ m n f
       return Off

instance Instrument tc () () (CreateST2Array r w On tc) where
  createInstrument _ _ () =
    wrapInstrument $ CreateST2Array $ \m n f ->
    let f' i =
          do On x <- f i
             return x
    in
    do xs <- parallelMP m n f'
       return (On xs)

-- | 'pmapST2ArrayMP' is a simple application of 'createST2Array'.  It
-- provides a similar interface to
-- 'Control.Monad.MultiPass.Utils.mapST2ArrayMP'. The difference is
-- that it only executes the map operation once the specified pass is
-- reached.
pmapST2ArrayMP
  :: (Ix i, Num i, Monad p1)
  => CreateST2Array r w p1 tc       -- ^ 'CreateST2Array' instrument
  -> NumThreads                     -- ^ Number of threads to spawn
  -> ST2Array r w i a               -- ^ Input array
  -> (a -> MultiPass r w tc (p1 b)) -- ^ Function to apply to each element
  -> MultiPass r w tc (p1 (ST2Array r w i b)) -- ^ Output array
pmapST2ArrayMP cr nThreads xs f =
  let f' i =
         do x <- readOnlyST2ToMP $ readST2Array xs i
            f x
  in
  do bnds <- readOnlyST2ToMP $ boundsST2Array xs
     createST2Array cr nThreads bnds f'
