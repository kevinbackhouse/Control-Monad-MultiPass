-- Copyright 2013 Kevin Backhouse.

{-|
The 'EmitST2Array' instrument is used to emit a sequence of values to
an 'ST2Array'. It has three passes. The first pass counts the number
of elements that will be written. The second pass is optional: it
enables the index values to be read before the actual values have been
written. (If this pass is not needed then the second and third passes
can be merged by coalescing the type variables for the second and
third passes: @EmitST2Array p1 p2 p2@.) The third pass writes the
values to the output array.
-}

module Control.Monad.MultiPass.Instrument.EmitST2Array
  ( EmitST2Array
  , setBaseIndex, emit, emitList, getIndex, getResult
  )
where

import Control.Exception ( assert )
import Control.Monad.ST2
import Control.Monad.MultiPass
import Control.Monad.MultiPass.ThreadContext.CounterTC
import Data.Ix

-- | Abstract datatype for the instrument.
data EmitST2Array i a r w p1 p2 p3 tc
  = EmitST2Array
      { setBaseInternal :: !(p2 i -> MultiPassPrologue r w tc ())

      , emitInternal :: !(p3 a -> MultiPass r w tc ())

      , emitListInternal :: !(p1 Int -> p3 [a] -> MultiPass r w tc ())

      , getIndexInternal :: !(forall w'. MultiPass r w' tc (p2 i))

      , getResultInternal
          :: !(MultiPassEpilogue r w tc (p3 (ST2Array r w i a)))
      }

-- | Initialise the base index of the output array. This method is
-- optional: if it is not called then the base index defaults to zero.
setBaseIndex
  :: (Ix i, Num i, Monad p1, Monad p2, Monad p3)
  => EmitST2Array i a r w p1 p2 p3 tc  -- ^ Instrument
  -> p2 i                              -- ^ Base index
  -> MultiPassPrologue r w tc ()
setBaseIndex =
  setBaseInternal

-- | Write one element to the output array.
emit
  :: (Ix i, Num i, Monad p1, Monad p2, Monad p3)
  => EmitST2Array i a r w p1 p2 p3 tc  -- ^ Instrument
  -> p3 a                              -- ^ Value to emit
  -> MultiPass r w tc ()
emit =
  emitInternal

-- | Write a list of elements to the output array. The length of the
-- list needs to be declared in the first pass so that the correct
-- number of elements can be allocated.
emitList
  :: (Ix i, Num i, Monad p1, Monad p2, Monad p3)
  => EmitST2Array i a r w p1 p2 p3 tc  -- ^ Instrument
  -> p1 Int                            -- ^ Length of the list
  -> p3 [a]                            -- ^ List of elements to emit
  -> MultiPass r w tc ()
emitList =
  emitListInternal

-- | Get the current index in the output array.
getIndex
  :: (Ix i, Num i, Monad p1, Monad p2, Monad p3)
  => EmitST2Array i a r w p1 p2 p3 tc  -- ^ Instrument
  -> MultiPass r w' tc (p2 i)          -- ^ Current index
getIndex =
  getIndexInternal

-- | Get the output array.
getResult
  :: (Ix i, Num i, Monad p1, Monad p2, Monad p3)
  => EmitST2Array i a r w p1 p2 p3 tc                 -- ^ Instrument
  -> MultiPassEpilogue r w tc (p3 (ST2Array r w i a)) -- ^ Output array
getResult =
  getResultInternal

newtype GC2 r w i
  = GC2 { gc2_base :: ST2Ref r w i }  -- Base index of the output array

data GC3 r w i a
  = GC3
      { gc3_base :: !(ST2Ref r w i)   -- Base index of the output array
      , gc3_output_array :: !(ST2Array r w i a)  -- Output array
      }

instance Instrument tc () ()
                    (EmitST2Array i a r w Off Off Off tc) where
  createInstrument _ _ () =
    wrapInstrument $
    EmitST2Array
      { setBaseInternal   = \Off -> return ()
      , emitInternal      = \Off -> return ()
      , emitListInternal  = \Off Off -> return ()
      , getIndexInternal  = return Off
      , getResultInternal = return Off
      }

instance Num i =>
         Instrument tc (CounterTC1 i r) ()
                    (EmitST2Array i a r w On Off Off tc) where
  createInstrument _ updateCtx () =
    wrapInstrument $
    EmitST2Array
      { setBaseInternal = \Off ->
          return ()

      , emitInternal = \Off ->
          mkMultiPass $
          do _ <- updateCtx incrCounterTC1
             return ()

      , emitListInternal = \(On n) Off ->
          mkMultiPass $
          do _ <- updateCtx (addkCounterTC1 (fromIntegral n))
             return ()

      , getIndexInternal =
          return Off

      , getResultInternal =
          return Off
      }

-- Pass 2 is optional. It can be included between passes 1 and 3, to
-- allow the getIndex method to be used before the values are written
-- into the array.
instance Num i =>
         Instrument tc (CounterTC2 i r) (GC2 r w i)
                    (EmitST2Array i a r w On On Off tc) where
  createInstrument st2ToMP updateCtx gc =
    wrapInstrument $
    EmitST2Array
      { setBaseInternal = \(On base) ->
          mkMultiPassPrologue $
          st2ToMP $ writeST2Ref (gc2_base gc) base

      , emitInternal = \Off ->
          mkMultiPass $
          do _ <- updateCtx incrCounterTC2
             return ()

      , emitListInternal = \(On n) Off ->
          mkMultiPass $
          do _ <- updateCtx (addkCounterTC2 (fromIntegral n))
             return ()

      , getIndexInternal =
          mkMultiPass $
          do counter <- updateCtx id
             base <- st2ToMP $ readST2Ref (gc2_base gc)
             return (On (base + counterVal2 counter))

      , getResultInternal =
          return Off
      }

instance (Ix i, Num i) =>
         Instrument tc (CounterTC2 i r) (GC3 r w i a)
                    (EmitST2Array i a r w On On On tc) where
  createInstrument st2ToMP updateCtx gc =
    wrapInstrument $
    EmitST2Array
      { setBaseInternal = \(On base) ->
          mkMultiPassPrologue $
          st2ToMP $ writeST2Ref (gc3_base gc) base

      , emitInternal = \(On x) ->
          mkMultiPass $
          do base <- st2ToMP $ readST2Ref (gc3_base gc)
             counter <- updateCtx incrCounterTC2
             let k = base + counterVal2 counter
             let xs = gc3_output_array gc
             st2ToMP $ writeST2Array xs k x

      , emitListInternal = \(On n) (On ys) ->
          assert (n == length ys) $
          mkMultiPass $
          do base <- st2ToMP $ readST2Ref (gc3_base gc)
             counter <- updateCtx (addkCounterTC2 (fromIntegral n))
             let k = base + counterVal2 counter
             sequence_
               [ let k' = k + fromIntegral i in
                 let xs = gc3_output_array gc in
                 st2ToMP $ writeST2Array xs k' y
               | (i,y) <- zip [0 .. n-1] ys
               ]

      , getIndexInternal =
          mkMultiPass $
          do base <- st2ToMP $ readST2Ref (gc3_base gc)
             counter <- updateCtx id
             return (On (base + counterVal2 counter))

      , getResultInternal =
          return $ On $ gc3_output_array gc
      }

-- This instrument never needs to back-track.
instance BackTrack r w (CounterTC2 i r) (GC2 r w i)
instance BackTrack r w (CounterTC2 i r) (GC3 r w i a)

instance Num i => NextGlobalContext r w tc () (GC2 r w i) where
  nextGlobalContext _ _ _ () =
    do base <- newST2Ref 0
       return $ GC2
         { gc2_base = base
         }

instance (Ix i, Num i) =>
         NextGlobalContext r w (CounterTC1 i r) (GC2 r w i)
                           (GC3 r w i a) where
  nextGlobalContext _ _ counter gc =
    do base <- readST2Ref (gc2_base gc)
       let n = base + counterVal1 counter
       xs <- newST2Array_ (base, n-1)
       return $ GC3
         { gc3_base = gc2_base gc
         , gc3_output_array = xs
         }

instance NextGlobalContext r w tc (GC2 r w i) (GC2 r w i) where
  nextGlobalContext _ _ _ gc =
    return gc

instance (Ix i, Num i) =>
         NextGlobalContext r w (CounterTC2 i r) (GC2 r w i)
                           (GC3 r w i a) where
  nextGlobalContext _ _ counter gc =
    do base <- readST2Ref (gc2_base gc)
       let n = base + counterVal2 counter
       xs <- newST2Array_ (base, n-1)
       return $ GC3
         { gc3_base = gc2_base gc
         , gc3_output_array = xs
         }

instance NextGlobalContext r w (CounterTC2 i r)
                           (GC3 r w i a) (GC3 r w i a)
                           where
  nextGlobalContext _ _ _ gc =
    return gc

instance NextGlobalContext r w (CounterTC2 i r)
                           (GC3 r w i a) (GC2 r w i)
                           where
  nextGlobalContext _ _ _ gc =
    return $ GC2 { gc2_base = gc3_base gc }
