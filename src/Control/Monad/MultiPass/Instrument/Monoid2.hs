-- Copyright 2013 Kevin Backhouse.

{-|
The 'Monoid2' instrument is used to accumulate a global value during
the first pass. During the second pass, the global value can be read
but not written. The value must be an instance of the
'Data.Monoid.Monoid' class. The names of the methods, 'tell' and
'listen', are taken from the 'Control.Monad.Writer.MonadWriter'
class. If this causes a naming conflict, then this module should be
imported qualified. For example:

> import qualified Control.Monad.MultiPass.Instrument.Monoid2 as M
-}

module Control.Monad.MultiPass.Instrument.Monoid2
  ( Monoid2
  , tell, listen
  , tellPrologue, listenEpilogue
  )
where

import Control.Monad ( void )
import Control.Monad.MultiPass
import Control.Monad.MultiPass.ThreadContext.MonoidTC
import Data.Monoid

-- | Abstract datatype for the instrument.
data Monoid2 a r w p1 p2 tc
  = Monoid2
      { tellInternal :: !(p1 a -> MultiPassBase r w tc ())
      , listenInternal :: !(MultiPass r w tc (p2 a))
      , listenInternalEpilogue :: !(MultiPassEpilogue r w tc (p1 a))
      }

-- | Add a value to the global value, during the first pass.
tell
  :: (Monoid a, Monad p1, Monad p2)
  => Monoid2 a r w p1 p2 tc   -- ^ Instrument
  -> p1 a                     -- ^ Value to add
  -> MultiPass r w tc ()
tell m v =
  mkMultiPass $ tellInternal m v

-- | Add a value to the global value, during the prologue of the first
-- pass.
tellPrologue
  :: (Monoid a, Monad p1, Monad p2)
  => Monoid2 a r w p1 p2 tc   -- ^ Instrument
  -> p1 a                     -- ^ Value to add
  -> MultiPassPrologue r w tc ()
tellPrologue m v =
  mkMultiPassPrologue $ tellInternal m v

-- | Read the global value, during the second pass.
listen
  :: (Monoid a, Monad p1, Monad p2)
  => Monoid2 a r w p1 p2 tc   -- ^ Instrument
  -> MultiPass r w tc (p2 a)  -- ^ Global value
listen =
  listenInternal

-- | Read the global value, during the epilogue of the first pass.
listenEpilogue
  :: (Monoid a, Monad p1, Monad p2)
  => Monoid2 a r w p1 p2 tc   -- ^ Instrument
  -> MultiPassEpilogue r w tc (p1 a)  -- ^ Global value
listenEpilogue =
  listenInternalEpilogue

-- Global context, used during the second phase.
newtype GC a
  = GC a

instance Instrument tc () () (Monoid2 a r w Off Off tc) where
  createInstrument _ _ () =
    wrapInstrument $
    Monoid2
      { tellInternal = \Off -> return ()
      , listenInternal = return Off
      , listenInternalEpilogue = return Off
      }

instance Monoid a =>
         Instrument tc (MonoidTC a) ()
                    (Monoid2 a r w On Off tc) where
  createInstrument _ updateCtx () =
    wrapInstrument $
    Monoid2
      { tellInternal = \(On x) ->
          void $ updateCtx (MonoidTC . mappend x . unwrapMonoidTC)

      , listenInternal =
          return Off

      , listenInternalEpilogue =
          mkMultiPassEpilogue $
          do MonoidTC x <- updateCtx id
             return (On x)
      }

instance Instrument tc () (GC a) (Monoid2 a r w On On tc) where
  createInstrument _ _ (GC x) =
    wrapInstrument $ Monoid2
      { tellInternal = \(On _) -> return ()
      , listenInternal = return $ On $ x
      , listenInternalEpilogue = return $ On $ x
      }

-- This instrument never needs to back-track.
instance BackTrack r w () (GC a)

instance NextGlobalContext r w (MonoidTC a) () (GC a) where
  nextGlobalContext _ _ (MonoidTC x) () =
    return (GC x)

instance NextGlobalContext r w () (GC a) (GC a) where
  nextGlobalContext _ _ () gc =
    return gc
