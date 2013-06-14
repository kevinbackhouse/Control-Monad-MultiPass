-- Copyright 2013 Kevin Backhouse.

{-|
For every new instrument, a number of class instances need to be
defined, such as 'NextGlobalContext' and 'NextThreadContext'. The
tests in this module are used to check that all the necessary
instances have been defined. Each test defines a trivial algorithm,
parameterised by an instrument of a specific arity. For example,
'testInstrument3' is parameterised by a three-pass instrument. The
test is used as follows:

> instanceTest :: ST2 r w ()
> instanceTest = run instanceTestBody
>
> instanceTestBody :: TestInstrument3 (MyInstrument r w) r w
> instanceTestBody = testInstrument3

If this code does not cause any compiler errors, then all the
necessary instances have been defined for @MyInstrument@.
-}

module Control.Monad.MultiPass.Utils.InstanceTest
         ( -- * Test for One-Pass Instruments
           testInstrument1, TestInstrument1

           -- * Test for Two-Pass Instruments
         , testInstrument2, TestInstrument2

           -- * Test for Three-Pass Instruments
         , testInstrument3, TestInstrument3

           -- * Test for Four-Pass Instruments
         , testInstrument4, TestInstrument4
         )
where

import Control.Monad.MultiPass


----------------------------------------------------------------------
------------------- Test for One-Pass Instruments --------------------
----------------------------------------------------------------------

-- | Test type for a one-pass instrument.
type TestInstrument1 f r w
  = PassS (PassS (PassS PassZ)) (WrappedType1 f r w)

-- | Test function for a one-pass instrument.
testInstrument1 :: TestInstrument1 f r w
testInstrument1 =
  PassS $ PassS $ PassS $
  PassZ $ WrappedType1 $ testBody1

type UnwrappedType1 f r w p1 p2 p3 tc
  =  f p1 tc
  -> f p2 tc
  -> f p3 tc
  -> MultiPassMain r w tc (p3 ())

newtype WrappedType1 f r w p1 p2 p3 tc =
  WrappedType1 (UnwrappedType1 f r w p1 p2 p3 tc)

instance MultiPassAlgorithm
           (WrappedType1 f r w p1 p2 p3 tc)
           (UnwrappedType1 f r w p1 p2 p3 tc)
           where
  unwrapMultiPassAlgorithm (WrappedType1 f) = f

testBody1
  :: Monad p3
  => UnwrappedType1 f r w p1 p2 p3 tc
testBody1 _ _ _ =
  mkMultiPassMain
    (return ())
    (\() -> return ())
    (\() -> return (return ()))


----------------------------------------------------------------------
------------------- Test for Two-Pass Instruments --------------------
----------------------------------------------------------------------

-- | Test type for a two-pass instrument.
type TestInstrument2 f r w
  = PassS (PassS (PassS (PassS PassZ))) (WrappedType2 f r w)

-- | Test function for a two-pass instrument.
testInstrument2 :: TestInstrument2 f r w
testInstrument2 =
  PassS $ PassS $ PassS $ PassS $
  PassZ $ WrappedType2 $ testBody2

type UnwrappedType2 f r w p1 p2 p3 p4 tc
  =  f p1 p2 tc
  -> f p3 p4 tc
  -> f p1 p3 tc
  -> f p2 p4 tc
  -> MultiPassMain r w tc (p4 ())

newtype WrappedType2 f r w p1 p2 p3 p4 tc =
  WrappedType2 (UnwrappedType2 f r w p1 p2 p3 p4 tc)

instance MultiPassAlgorithm
           (WrappedType2 f r w p1 p2 p3 p4 tc)
           (UnwrappedType2 f r w p1 p2 p3 p4 tc)
           where
  unwrapMultiPassAlgorithm (WrappedType2 f) = f

testBody2
  :: Monad p4
  => UnwrappedType2 f r w p1 p2 p3 p4 tc
testBody2 _ _ _ _ =
  mkMultiPassMain
    (return ())
    (\() -> return ())
    (\() -> return (return ()))


----------------------------------------------------------------------
------------------ Test for Three-Pass Instruments -------------------
----------------------------------------------------------------------

-- | Test type for a three-pass instrument.
type TestInstrument3 f r w
  = PassS (PassS (PassS (PassS (PassS (PassS PassZ)))))
          (WrappedType3 f r w)

-- | Test function for a three-pass instrument.
testInstrument3 :: TestInstrument3 f r w
testInstrument3 =
  PassS $ PassS $ PassS $ PassS $ PassS $ PassS $
  PassZ $ WrappedType3 $ testBody3

type UnwrappedType3 f r w p1 p2 p3 p4 p5 p6 tc
  =  f p1 p2 p3 tc
  -> f p4 p5 p6 tc
  -> f p1 p3 p4 tc
  -> f p2 p4 p6 tc
  -> MultiPassMain r w tc (p6 ())

newtype WrappedType3 f r w p1 p2 p3 p4 p5 p6 tc =
  WrappedType3 (UnwrappedType3 f r w p1 p2 p3 p4 p5 p6 tc)

instance MultiPassAlgorithm
           (WrappedType3 f r w p1 p2 p3 p4 p5 p6 tc)
           (UnwrappedType3 f r w p1 p2 p3 p4 p5 p6 tc)
           where
  unwrapMultiPassAlgorithm (WrappedType3 f) = f

testBody3
  :: Monad p6
  => UnwrappedType3 f r w p1 p2 p3 p4 p5 p6 tc
testBody3 _ _ _ _ =
  mkMultiPassMain
    (return ())
    (\() -> return ())
    (\() -> return (return ()))


----------------------------------------------------------------------
------------------- Test for Four-Pass Instruments -------------------
----------------------------------------------------------------------

-- | Test type for a four-pass instrument.
type TestInstrument4 f r w
  = PassS (PassS (PassS (PassS (PassS (PassS (PassS (PassS PassZ)))))))
          (WrappedType4 f r w)

-- | Test function for a four-pass instrument.
testInstrument4 :: TestInstrument4 f r w
testInstrument4 =
  PassS $ PassS $ PassS $ PassS $ PassS $ PassS $ PassS $ PassS $
  PassZ $ WrappedType4 $ testBody4

type UnwrappedType4 f r w p1 p2 p3 p4 p5 p6 p7 p8 tc
  =  f p1 p2 p3 p4 tc
  -> f p5 p6 p7 p8 tc
  -> f p1 p3 p5 p7 tc
  -> f p2 p4 p6 p8 tc
  -> MultiPassMain r w tc (p8 ())

newtype WrappedType4 f r w p1 p2 p3 p4 p5 p6 p7 p8 tc =
  WrappedType4 (UnwrappedType4 f r w p1 p2 p3 p4 p5 p6 p7 p8 tc)

instance MultiPassAlgorithm
           (WrappedType4 f r w p1 p2 p3 p4 p5 p6 p7 p8 tc)
           (UnwrappedType4 f r w p1 p2 p3 p4 p5 p6 p7 p8 tc)
           where
  unwrapMultiPassAlgorithm (WrappedType4 f) = f

testBody4
  :: Monad p8
  => UnwrappedType4 f r w p1 p2 p3 p4 p5 p6 p7 p8 tc
testBody4 _ _ _ _ =
  mkMultiPassMain
    (return ())
    (\() -> return ())
    (\() -> return (return ()))
