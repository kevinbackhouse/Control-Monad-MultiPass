-- Copyright 2013 Kevin Backhouse.

{-# OPTIONS_GHC -XKindSignatures #-}

{-|
The 'Delay' instrument is stateless and its implementation is trivial.
Its purpose is to allow values which were computed in pass @p1@ to be
used in pass @p2@.
-}

module Control.Monad.MultiPass.Instrument.Delay
  ( Delay
  , delay
  )
where

import Control.Monad.MultiPass

-- | Abstract datatype for the instrument.
data Delay p1 p2 (tc :: *)
  = Delay { delayInternal :: !(forall (a :: *). p1 a -> p2 a) }

-- | 'delay' enables a value which was computed in pass @p1@ to be
-- used in pass @p2@.
delay :: Delay p1 p2 tc -> p1 a -> p2 a
delay =
  delayInternal

instance Instrument tc () () (Delay Off Off tc) where
  createInstrument _ _ () =
    wrapInstrument $ Delay $ \Off -> Off

instance Instrument tc () () (Delay On Off tc) where
  createInstrument _ _ () =
    wrapInstrument $ Delay $ \(On _) -> Off

instance Instrument tc () () (Delay On On tc) where
  createInstrument _ _ () =
    wrapInstrument $ Delay $ id
