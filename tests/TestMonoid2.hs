-- Copyright 2013 Kevin Backhouse.

module TestMonoid2 ( instanceTest ) where

import Control.Monad.ST2
import Control.Monad.MultiPass
import Control.Monad.MultiPass.Instrument.Monoid2
import Control.Monad.MultiPass.Utils.InstanceTest
import Data.Monoid

-- This test checks that all the necessary instances have been
-- defined. Its only purpose is to check that there are no compile
-- errors, so it does not need to be executed.
instanceTest :: ST2 r w ()
instanceTest =
  do instanceTest1
     instanceTest2

instanceTest1 :: ST2 r w ()
instanceTest1 = run instanceTestBody1

instanceTestBody1 :: TestInstrument2 (Monoid2 Any r w) r w
instanceTestBody1 = testInstrument2

instanceTest2 :: ST2 r w ()
instanceTest2 = run instanceTestBody2

instanceTestBody2 :: TestInstrument2 (Monoid2 All r w) r w
instanceTestBody2 = testInstrument2
