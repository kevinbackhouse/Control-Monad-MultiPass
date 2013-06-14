-- Copyright 2013 Kevin Backhouse.

{-# OPTIONS_GHC -XKindSignatures #-}

{-|
The 'Knot3' instrument is used for knot tying across passes. Knot
tying is a technique sometimes used in lazy functional programming, in
which the definition of a variable depends on its own value. The lazy
programming technique depends on an implicit two-pass ordering of the
computation. For example, the classic repmin program produces a pair
of outputs - a tree and an integer - and there is an implicit two-pass
ordering where the integer is computed during the first pass and the
tree during the second. The 'Knot3' instrument allows the same
technique to be applied, but the ordering of the passes is managed
explicitly by the "Control.Monad.MultiPass" library, rather than
implicitly by lazy evalution.
-}

module Control.Monad.MultiPass.Instrument.Knot3
  ( Knot3
  , knot3
  )
where

import Control.Monad ( void )
import Control.Monad.ST2
import Control.Monad.MultiPass
import Control.Monad.MultiPass.ThreadContext.CounterTC

-- | Abstract datatype for the instrument.
data Knot3 (a :: *) r w (p1 :: * -> *) p2 p3 tc
  = Knot3
      { knot3Internal :: !(forall b.
          (p3 a -> MultiPass r w tc (p2 a, b)) -> MultiPass r w tc b)
      }

-- | Tie the knot for the supplied function.
knot3
  :: (Monad p1, Monad p2, Monad p3)
  => Knot3 a r w p1 p2 p3 tc
  -> (p3 a -> MultiPass r w tc (p2 a, b))
  -> MultiPass r w tc b
knot3 =
  knot3Internal

newtype Buffer r w a
  = Buffer (ST2Array r w Int a)  -- Storage array

instance Instrument tc () () (Knot3 a r w Off Off Off tc) where
  createInstrument _ _ () =
    wrapInstrument $ Knot3 $ \f ->
    do (Off, x) <- f Off
       return x

-- Pass 1 of the Knot3 instrument. This pass counts the number of
-- times knot3 is used, so that an array can be allocated to store the
-- values during the second pass.
instance Instrument tc (CounterTC1 Int r) ()
                    (Knot3 a r w On Off Off tc) where
  createInstrument _ updateCtx () =
    wrapInstrument $ Knot3 $ \f ->
    do void $ mkMultiPass $ updateCtx incrCounterTC1
       (Off, x) <- f Off
       return x

instance Instrument tc (CounterTC2 Int r) (Buffer r w a)
                    (Knot3 a r w On On Off tc) where
  createInstrument st2ToMP updateCtx (Buffer xs) =
    wrapInstrument $ Knot3 $ \f ->
    do counter <- mkMultiPass $ updateCtx incrCounterTC2
       let k = counterVal2 counter
       (On v, x) <- f Off
       mkMultiPass $ st2ToMP $ writeST2Array xs k v
       return x

instance Instrument tc (CounterTC2 Int r) (Buffer r w a)
                    (Knot3 a r w On On On tc) where
  createInstrument st2ToMP updateCtx (Buffer xs) =
    wrapInstrument $ Knot3 $ \f ->
    do counter <- mkMultiPass $ updateCtx incrCounterTC2
       let k = counterVal2 counter
       v <- mkMultiPass $ st2ToMP $ readST2Array xs k
       (_,x) <- f (On v)
       return x

-- This instrument never needs to back-track.
instance BackTrack r w tc (Buffer r w a)

instance NextGlobalContext r w (CounterTC1 Int r)
                           () (Buffer r w a) where
  nextGlobalContext _ _ counter () =
    let n = counterVal1 counter in
    do xs <- newST2Array_ (0, n-1)
       return (Buffer xs)

instance NextGlobalContext r w tc (Buffer r w a)
                           (Buffer r w a) where
  nextGlobalContext _ _ _ (Buffer xs) = return (Buffer xs)
