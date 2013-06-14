-- Copyright 2013 Kevin Backhouse.

{-|
The 'EmitST2ArrayFxp' instrument has an identical interface to
'Control.Monad.MultiPass.Instrument.EmitST2Array'. The only difference
is that 'EmitST2ArrayFxp' includes support for back-tracking. The
'emitList' method of 'EmitST2ArrayFxp' permits the list argument to be
longer than the lower bound which was specified during the first
pass. If it is then the algorithm will back-track to the beginning of
the second pass and iterate until a fixed point has been reached.
-}

module Control.Monad.MultiPass.Instrument.EmitST2ArrayFxp
  ( EmitST2ArrayFxp
  , setBaseIndex, emit, emitList, getIndex, getResult
  )
where

import Control.Exception ( assert )
import Control.Monad.ST2
import Control.Monad.Writer.Strict
import Control.Monad.MultiPass
import Control.Monad.MultiPass.Utils.UpdateCtx
import Control.Monad.MultiPass.ThreadContext.CounterTC
import Control.Monad.MultiPass.ThreadContext.MonoidTC
import Data.Ix

-- | Abstract datatype for the instrument.
data EmitST2ArrayFxp i a r w p1 p2 p3 tc
  = EmitST2ArrayFxp
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
  => EmitST2ArrayFxp i a r w p1 p2 p3 tc  -- ^ Instrument
  -> p2 i                                 -- ^ Base index
  -> MultiPassPrologue r w tc ()
setBaseIndex =
  setBaseInternal

-- | Write one element to the output array.
emit
  :: (Ix i, Num i, Monad p1, Monad p2, Monad p3)
  => EmitST2ArrayFxp i a r w p1 p2 p3 tc  -- ^ Instrument
  -> p3 a                                 -- ^ Value to emit
  -> MultiPass r w tc ()
emit =
  emitInternal

-- | Write a list of elements to the output array. The instrument uses
-- back-tracking to iterate until the length of the list has been
-- determined. It is the client's responsibility to ensure that any
-- operations which depend on the length of the list are monotonic so
-- that a fixed point will be found. The first argument is used to
-- supply a minimum length for the list (zero is always a valid
-- input). It can be used to shorten the time to convergence when a
-- good lower bound is known.
emitList
  :: (Ix i, Num i, Monad p1, Monad p2, Monad p3)
  => EmitST2ArrayFxp i a r w p1 p2 p3 tc  -- ^ Instrument
  -> p1 Int                               -- ^ Length of the list
  -> p3 [a]                               -- ^ List of elements to emit
  -> MultiPass r w tc ()
emitList =
  emitListInternal

-- | Get the current index in the output array.
getIndex
  :: (Ix i, Num i, Monad p1, Monad p2, Monad p3)
  => EmitST2ArrayFxp i a r w p1 p2 p3 tc  -- ^ Instrument
  -> MultiPass r w' tc (p2 i)             -- ^ Current index
getIndex =
  getIndexInternal

-- | Get the output array.
getResult
  :: (Ix i, Num i, Monad p1, Monad p2, Monad p3)
  => EmitST2ArrayFxp i a r w p1 p2 p3 tc              -- ^ Instrument
  -> MultiPassEpilogue r w tc (p3 (ST2Array r w i a)) -- ^ Output array
getResult =
  getResultInternal

instance Instrument tc () ()
                    (EmitST2ArrayFxp i a r w Off Off Off tc) where
  createInstrument _ _ () =
    wrapInstrument $
    EmitST2ArrayFxp
      { setBaseInternal   = \Off -> return ()
      , emitInternal      = \Off -> return ()
      , emitListInternal  = \Off Off -> return ()
      , getIndexInternal  = return Off
      , getResultInternal = return Off
      }

-- Thread context for the first pass. One first counter is for the
-- current index. The second counter is for the number of calls to
-- emitList.
type TC1 i r = (CounterTC1 i r, CounterTC1 ListIndex r)

newtype ListIndex
  = ListIndex Int
    deriving (Eq,Ord,Ix)

instance Num ListIndex where
  (ListIndex x) + (ListIndex y) = ListIndex (x + y)
  (ListIndex x) - (ListIndex y) = ListIndex (x - y)
  (ListIndex x) * (ListIndex y) = ListIndex (x * y)
  negate (ListIndex x) = ListIndex (negate x)
  abs (ListIndex x) = ListIndex (abs x)
  signum (ListIndex x) = ListIndex (signum x)
  fromInteger x = ListIndex (fromInteger x)

instance Show ListIndex where
  show (ListIndex i) = show i

instance Num i =>
         Instrument tc (TC1 i r) ()
                    (EmitST2ArrayFxp i a r w On Off Off tc) where
  createInstrument _ updateCtx () =
    wrapInstrument $
    EmitST2ArrayFxp
      { setBaseInternal = \Off ->
          return ()

      , emitInternal = \Off ->
          mkMultiPass $
          do _ <- updateCtxFst updateCtx incrCounterTC1
             return ()

      , emitListInternal = \(On lowerBound) Off ->
          mkMultiPass $
          do _ <- updateCtxFst updateCtx
                    (addkCounterTC1 (fromIntegral lowerBound))
             _ <- updateCtxSnd updateCtx incrCounterTC1
             return ()

      , getIndexInternal =
          return Off

      , getResultInternal =
          return Off
      }

-- Thread context for the second pass. The fields correspond to the
-- fields of TC1.
type TC2 i r = (CounterTC2 i r, CounterTC2 ListIndex r)

data GC2 r w i
  = GC2 { -- Base index of the output array
          gc2_base :: !(ST2Ref r w i)

          -- The length array is uninitialised the first time the
          -- pass is executed.
        , gc2_initialised :: !Bool

          -- Array of list lengths.
        , gc2_length_array :: !(ST2Array r w ListIndex Int)

          -- The pass number for the second pass.
        , gc2_passnumber :: !PassNumber
        }

-- Pass 2 is optional. It can be included between passes 1 and 3, to
-- allow the getIndexInternal method to be used before the values are
-- written into the array.
instance (Ix i, Num i) =>
         Instrument tc (TC2 i r) (GC2 r w i)
                    (EmitST2ArrayFxp i a r w On On Off tc) where
  createInstrument st2ToMP updateCtx gc =
    wrapInstrument $
    EmitST2ArrayFxp
      { setBaseInternal = \(On base) ->
          mkMultiPassPrologue $
          st2ToMP $ writeST2Ref (gc2_base gc) base

      , emitInternal = \Off ->
          void $ mkMultiPass $ updateCtxFst updateCtx incrCounterTC2

      , emitListInternal =
          -- The initialised field specifies whether the elements of
          -- the length array have been initialised yet.
          let lenArray = gc2_length_array gc in
          if gc2_initialised gc then (
            \(On lowerBound) Off ->
            mkMultiPass $
            do listCount <- updateCtxSnd updateCtx incrCounterTC2
               let i = counterVal2 listCount
               len <- st2ToMP $ readST2Array lenArray i
               assert (lowerBound <= len) $ return ()
               void $ updateCtxFst updateCtx $
                      addkCounterTC2 (fromIntegral len)
          ) else (
            \(On lowerBound) Off ->
            mkMultiPass $
            do listCount <- updateCtxSnd updateCtx incrCounterTC2
               let i = counterVal2 listCount
               -- Initialise the length array with the lower bound.
               st2ToMP $ writeST2Array lenArray i lowerBound
               void $ updateCtxFst updateCtx $
                      addkCounterTC2 (fromIntegral lowerBound)
          )

      , getIndexInternal =
          mkMultiPass $
          do base <- st2ToMP $ readST2Ref (gc2_base gc)
             counter <- updateCtxFst updateCtx id
             return (On (base + counterVal2 counter))

      , getResultInternal =
          return Off
      }

-- Thread context for the third pass. The indexCounter and listCounter
-- fields correspond to the counters in the TC1 and TC2 thread
-- contexts. The third field, newIndexCounter, will become the index
-- counter if the algorithm back-tracks. The fourth field,
-- indexChanged, tracks whether there are any differences between the
-- new counter and the old.
data TC3 i r
  = TC3 { indexCounter    :: CounterTC2 i r
        , listCounter     :: CounterTC2 ListIndex r
        , newIndexCounter :: CounterTC1 i r
        , indexChanged    :: MonoidTC Any
        }

updateIndexCounter
  :: UpdateThreadContext rootTC (TC3 i r)
  -> UpdateThreadContext rootTC (CounterTC2 i r)
updateIndexCounter updateCtx f =
  do tc <- updateCtx $ \tc ->
             tc { indexCounter = f (indexCounter tc) }
     return (indexCounter tc)

updateListCounter
  :: UpdateThreadContext rootTC (TC3 i r)
  -> UpdateThreadContext rootTC (CounterTC2 ListIndex r)
updateListCounter updateCtx f =
  do tc <- updateCtx $ \tc ->
             tc { listCounter = f (listCounter tc) }
     return (listCounter tc)

updateNewIndexCounter
  :: UpdateThreadContext rootTC (TC3 i r)
  -> UpdateThreadContext rootTC (CounterTC1 i r)
updateNewIndexCounter updateCtx f =
  do tc <- updateCtx $ \tc ->
             tc { newIndexCounter = f (newIndexCounter tc) }
     return (newIndexCounter tc)

updateIndexChanged
  :: UpdateThreadContext rootTC (TC3 i r)
  -> UpdateThreadContext rootTC (MonoidTC Any)
updateIndexChanged updateCtx f =
  do tc <- updateCtx $ \tc ->
             tc { indexChanged = f (indexChanged tc) }
     return (indexChanged tc)

instance Num i => ThreadContext r w (TC3 i r) where
  splitThreadContext m t (TC3 a b c d) =
    do a' <- splitThreadContext m t a
       b' <- splitThreadContext m t b
       c' <- splitThreadContext m t c
       d' <- splitThreadContext m t d
       return (TC3 a' b' c' d')

  mergeThreadContext m getSubContext (TC3 a b c d) =
    let getField f tc =
          do tc' <- getSubContext tc
             return (f tc')
    in
    do a' <- mergeThreadContext m (getField indexCounter) a
       b' <- mergeThreadContext m (getField listCounter) b
       c' <- mergeThreadContext m (getField newIndexCounter) c
       d' <- mergeThreadContext m (getField indexChanged) d
       return $ TC3
         { indexCounter    = a'
         , listCounter     = b'
         , newIndexCounter = c'
         , indexChanged    = d'
         }

data GC3 r w i a
  = GC3 { -- Base index of the output array
          gc3_base :: !(ST2Ref r w i)

          -- Array of list lengths.
        , gc3_length_array :: !(ST2Array r w ListIndex Int)

          -- Output array. This array is not allocated until the
          -- length array has reached a fixed point, so that its size
          -- is known.
        , gc3_output_array :: !(ST2Array r w i a)

          -- This field is False until a fixed point has been reached.
          -- It is used to indicate that the output array is ready to
          -- be written. (When gc3_ready is False, the output array is
          -- empty.)
        , gc3_ready :: !Bool

          -- The pass number for the second pass, unless the second
          -- pass was skipped, in which case it is the pass number for
          -- the third pass.
        , gc3_passnumber2 :: !PassNumber

          -- The pass number for the third pass.
        , gc3_passnumber3 :: !PassNumber
        }

instance (Ix i, Num i) =>
         Instrument tc (TC3 i r) (GC3 r w i a)
                    (EmitST2ArrayFxp i a r w On On On tc) where
  createInstrument st2ToMP updateCtx gc =
    -- This function updates the indexCounter and newIndexCounter and
    -- returns the current index.
    let writeHelper =
          do void $ updateNewIndexCounter updateCtx incrCounterTC1
             base <- st2ToMP $ readST2Ref (gc3_base gc)
             counter <-
               updateIndexCounter updateCtx incrCounterTC2
             return $ base + counterVal2 counter
    in
    -- This function updates the listCounter, indexCounter, and
    -- newIndexCounter and checks whether a fixed point has been
    -- reached. It returns the current index.
    let writeListHelper lowerBound ys =
          let newLen = length ys in
          do -- Update the list length array and check whether the
             -- length has increased since the last iteration.
             listCount <-
               updateListCounter updateCtx incrCounterTC2
             let i = counterVal2 listCount
             oldLen <- st2ToMP $ readST2Array (gc3_length_array gc) i
             st2ToMP $ writeST2Array (gc3_length_array gc) i newLen
             -- Update the indexChanged field.
             assert (newLen >= lowerBound) $ return ()
             assert (newLen >= oldLen) $ return ()
             let changed = MonoidTC $ Any $ newLen /= oldLen
             void $ updateIndexChanged updateCtx $ mappend changed
             -- Update the newIndexCounter field with the new length.
             void $ updateNewIndexCounter updateCtx
                      (addkCounterTC1 (fromIntegral newLen))
             -- Get the current count. The indexCounter field was
             -- initialised with the old length, so it needs to be
             -- used again here for consistency.
             base <- st2ToMP $ readST2Ref (gc3_base gc)
             indexCount <- updateIndexCounter updateCtx
                             (addkCounterTC2 (fromIntegral oldLen))
             return (base + counterVal2 indexCount)
    in
    let setBaseHelper (On base) =
          mkMultiPassPrologue $
          st2ToMP $ writeST2Ref (gc3_base gc) base
    in
    let getIndexHelper =
          mkMultiPass $
          do base <- st2ToMP $ readST2Ref (gc3_base gc)
             indexCount <- updateIndexCounter updateCtx id
             return (On (base + counterVal2 indexCount))
    in
    let xs = gc3_output_array gc in
    let getResultHelper = return $ On $ xs in
    -- The code below creates two different versions of the
    -- instrument, depending on the value of gc3_ready. If gc3_ready
    -- is false then no values are written to the output array because
    -- it has not been allocated yet.
    if gc3_ready gc then (
      wrapInstrument $
           EmitST2ArrayFxp
           { setBaseInternal = setBaseHelper

           , emitInternal = \(On x) ->
               mkMultiPass $
               do k <- writeHelper
                  st2ToMP $ writeST2Array xs k x

           , emitListInternal = \(On lowerBound) (On ys) ->
               mkMultiPass $
               do j <- writeListHelper lowerBound ys
                  let n = length ys
                  sequence_
                    [ let j' = j + fromIntegral k in
                      st2ToMP $ writeST2Array xs j' y
                    | (k,y) <- zip [0 .. n-1] ys
                    ]

           , getIndexInternal = getIndexHelper
           , getResultInternal = getResultHelper
           }
    ) else (
      wrapInstrument $
           EmitST2ArrayFxp
           { setBaseInternal = setBaseHelper

           , emitInternal = \(On _) ->
               void $ mkMultiPass $ writeHelper

           , emitListInternal = \(On lowerBound) (On ys) ->
              void $ mkMultiPass $ writeListHelper lowerBound ys

           , getIndexInternal = getIndexHelper
           , getResultInternal = getResultHelper
           }
    )

-- This instrument never needs to back-track after the second pass.
instance BackTrack r w (TC2 i r) (GC2 r w i)

instance BackTrack r w (TC3 i r) (GC3 r w i a) where
  backtrack tc gc =
    let MonoidTC (Any changed) = indexChanged tc in
    case (changed, gc3_ready gc) of
      (False, False)
        -> -- A fixed point has been found, but the output array has
           -- not been created yet, so the current pass needs to be
           -- executed one more time.
           return $ Just $ gc3_passnumber3 gc

      (False, True)
        -> -- A fixed point has already been found and the array has
           -- already been allocated, so there is no need to
           -- back-track.
           return Nothing

      (True, False)
        -> -- A fixed point has not been found yet, so back-track to
           -- the second pass.
           return $ Just $ gc3_passnumber2 gc

      (True, True)
        -> -- A fixed point has not been found yet, so the array
           -- should not have been allocated yet.
           assert False $ return Nothing

instance Num i =>
         NextThreadContext r w (TC3 i r) gc (TC3 i r) where
  nextThreadContext _ _ tc _ =
    do -- Replace the old index counter with the new index counter.
       indexCount <- newCounterTC2 (newIndexCounter tc)
       return $ TC3
         { indexCounter    = indexCount
         , listCounter     = resetCounterTC2 (listCounter tc)
         , newIndexCounter = newCounterTC1
         , indexChanged    = mempty
         }

instance Num i =>
         NextGlobalContext r w (TC1 i r) () (GC2 r w i) where
  nextGlobalContext n _ (_,listCount) () =
    do base <- newST2Ref 0
       xs <- newST2Array_ (0, counterVal1 listCount - 1)
       return $ GC2
         { gc2_base         = base
         , gc2_initialised  = False
         , gc2_length_array = xs
         , gc2_passnumber   = n
         }

instance NextGlobalContext r w (TC2 i r) (GC2 r w i) (GC2 r w i) where
  nextGlobalContext _ _ _ gc =
    return $ gc { gc2_initialised = True }

instance (Ix i, Num i) =>
         NextGlobalContext r w (TC2 i r) (GC2 r w i) (GC3 r w i a) where
  nextGlobalContext n _ _ gc =
    do -- Initialise the output array with a trivial array. (The
       -- output array is not used when gc3_ready is False.)
       xs <- newST2Array_ (0,0)
       return $ GC3
         { gc3_base         = gc2_base gc
         , gc3_length_array = gc2_length_array gc
         , gc3_output_array = xs
         , gc3_ready        = False
         , gc3_passnumber2  = gc2_passnumber gc
         , gc3_passnumber3  = n
         }

instance (Ix i, Num i) =>
         NextGlobalContext r w (TC3 i r)
                           (GC3 r w i a) (GC3 r w i a) where
  nextGlobalContext _ StepForward _ gc = return gc
  nextGlobalContext _ StepBackward _ gc = return gc
  nextGlobalContext _ StepReset tc gc =
    let MonoidTC (Any changed) = indexChanged tc in
    case (changed, gc3_ready gc) of
      (False, False)
        -> -- A fixed point has been found, so it is time to
           -- allocate the array.
           do base <- readST2Ref (gc3_base gc)
              let n = base + counterVal2 (indexCounter tc)
              xs <- newST2Array_ (base, n-1)
              return $ gc
                { gc3_output_array = xs
                , gc3_ready        = True
                }

      (False, True)
        -> -- A fixed point has already been found and the array has
           -- already been allocated, so no change is needed.
           return gc

      (True, False)
        -> -- A fixed point has not been found yet, so no change is
           -- needed.
           return gc

      (True, True)
        -> -- A fixed point has not been found yet, so the array
           -- should not have been allocated yet.
           assert False $ return gc

instance NextGlobalContext r w (TC3 i r) (GC3 r w i a)
                           (GC2 r w i) where
  nextGlobalContext _ _ _ gc =
    return $ GC2
      { gc2_base         = gc3_base gc
      , gc2_initialised  = True
      , gc2_length_array = gc3_length_array gc
      , gc2_passnumber   = gc3_passnumber2 gc
      }

instance Num i =>
         NextThreadContext r w (TC2 i r) gc (TC3 i r) where
  nextThreadContext _ _ (indexCount, listCount) _ =
    return $ TC3
      { indexCounter    = resetCounterTC2 indexCount
      , listCounter     = resetCounterTC2 listCount
      , newIndexCounter = newCounterTC1
      , indexChanged    = mempty
      }

instance NextThreadContext r w (TC3 i r) gc (TC2 i r) where
  nextThreadContext _ _ tc _ =
    return (indexCounter tc, listCounter tc)
