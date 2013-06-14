-- Copyright 2013 Kevin Backhouse.

{-|
The 'DelayedLift' instrument is stateless and provides a similar
interface to 'readOnlyST2ToMP'. The difference is that it only
executes the read-only computation once the specified pass is reached.
-}

module Control.Monad.MultiPass.Instrument.DelayedLift
  ( DelayedLift
  , delayedLift, readST2ArrayMP
  )
where

import Control.Monad.ST2
import Control.Monad.MultiPass
import Data.Ix

-- | Abstract datatype for the instrument.
data DelayedLift r w p1 tc
  = DelayedLift
      { delayedLiftInternal ::
          !(forall a. p1 (ReadOnlyST2 r a) -> MultiPass r w tc (p1 a))
      }

-- | Execute the read-only computation during pass @p1@.
delayedLift
  :: Monad p1
  => DelayedLift r w p1 tc
  -> p1 (ReadOnlyST2 r a)
  -> MultiPass r w tc (p1 a)
delayedLift =
  delayedLiftInternal

instance Instrument tc () () (DelayedLift r w Off tc) where
  createInstrument _ _ () =
    wrapInstrument $ DelayedLift $ \Off ->
    return Off

instance Instrument tc () () (DelayedLift r w On tc) where
  createInstrument st2ToMP _ () =
    wrapInstrument $ DelayedLift $ \(On m) ->
    do x <- mkMultiPass $ st2ToMP $ runReadOnlyST2 m
       return (On x)

-- | 'readST2ArrayMP' is a simple application of 'delayedLift'. It
-- reads an index of the array during pass @p1@. This is particularly
-- useful if the array does not exist in earlier passes, for example
-- because it was created by the
-- 'Control.Monad.MultiPass.Instrument.CreateST2Array.CreateST2Array'
-- instrument.
readST2ArrayMP
  :: (Ix i, Monad p1)
  => DelayedLift r w p1 tc
  -> p1 (ST2Array r w i a)
  -> i
  -> MultiPass r w tc (p1 a)
readST2ArrayMP dlift xs i =
  delayedLift dlift $
  do xs' <- xs
     return (ReadOnlyST2 $ readST2Array xs' i)
