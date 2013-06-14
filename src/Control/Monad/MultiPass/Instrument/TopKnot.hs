-- Copyright 2013 Kevin Backhouse.

{-|
The 'TopKnot' instrument is used for knot tying across passes. It
allows a value to be written during the epilogue of one pass and read
during the prologue of a later pass.  Knot tying is a technique
sometimes used in lazy functional programming, in which the definition
of a variable depends on its own value. The lazy programming technique
depends on an implicit two-pass ordering of the computation. For
example, the classic repmin program produces a pair of outputs - a
tree and an integer - and there is an implicit two-pass ordering where
the integer is computed during the first pass and the tree during the
second. The 'TopKnot' instrument allows the same technique to be
applied, but the ordering of the passes is managed explicitly by the
"Control.Monad.MultiPass" library, rather than implicitly by lazy
evalution.
-}

module Control.Monad.MultiPass.Instrument.TopKnot
  ( TopKnot
  , load, store
  )
where

import Control.Exception ( assert )
import Control.Monad.ST2
import Control.Monad.MultiPass
import Data.Maybe ( isNothing, isJust, fromJust )

-- | Abstract datatype for the instrument.
data TopKnot a r w p1 p2 tc
  = TopKnot
      { loadInternal :: MultiPassPrologue r w tc (p2 a)
      , storeInternal :: (p1 a) -> MultiPassEpilogue r w tc ()
      }

-- | Load the value that was stored during the first pass.
load :: TopKnot a r w p1 p2 tc -> MultiPassPrologue r w tc (p2 a)
load =
  loadInternal

-- | Store a value during the epilogue of the first pass. This
-- function should be called exactly once.
store :: TopKnot a r w p1 p2 tc -> p1 a -> MultiPassEpilogue r w tc ()
store =
  storeInternal

-- Global Context.
newtype GC r w a
  = GC (ST2Ref r w (Maybe a))

instance Instrument tc () () (TopKnot a r w Off Off tc) where
  createInstrument _ _ () =
    wrapInstrument $ TopKnot
      { loadInternal = return Off
      , storeInternal = \Off -> return ()
      }

-- First pass of the TopKnot instrument. The storeInternal method is
-- expected to be called exactly once during this pass.
instance Instrument tc () (GC r w a) (TopKnot a r w On Off tc) where
  createInstrument st2ToMP _ (GC r) =
    wrapInstrument $ TopKnot
      { loadInternal = return Off

      , storeInternal = \(On x) ->
          mkMultiPassEpilogue $ st2ToMP $
          do mx <- readST2Ref r
             assert (isNothing mx) $ return ()
             writeST2Ref r (Just x)
      }

-- Second pass of the TopKnot instrument.
instance Instrument tc () (GC r w a) (TopKnot a r w On On tc) where
  createInstrument st2ToMP _ (GC r) =
    wrapInstrument $ TopKnot
      { loadInternal =
          mkMultiPassPrologue $ st2ToMP $
          do mx <- readST2Ref r
             assert (isJust mx) $ return ()
             return $ On $ fromJust mx

      , storeInternal = \(On x) ->
          mkMultiPassEpilogue $ st2ToMP $
          do mx <- readST2Ref r
             assert (isNothing mx) $ return ()
             writeST2Ref r (Just x)
      }

-- This instrument never needs to back-track.
instance BackTrack r w tc (GC r w a)

instance NextGlobalContext r w () () (GC r w a) where
  nextGlobalContext _ _ () () =
    do mx <- newST2Ref Nothing
       return (GC mx)

instance NextGlobalContext r w () (GC r w a) (GC r w a) where
  nextGlobalContext _ _ () gc = return gc
