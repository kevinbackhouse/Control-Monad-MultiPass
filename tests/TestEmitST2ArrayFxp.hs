-- Copyright 2013 Kevin Backhouse.

module TestEmitST2ArrayFxp ( instanceTest ) where

import Control.Monad.ST2
import Control.Monad.MultiPass
import Control.Monad.MultiPass.Instrument.EmitST2ArrayFxp
import Control.Monad.MultiPass.Utils.InstanceTest

-- This test checks that all the necessary instances have been
-- defined. Its only purpose is to check that there are no compile
-- errors, so it does not need to be executed.
instanceTest :: ST2 r w ()
instanceTest =
  do instanceTest1
     instanceTest2

instanceTest1 :: ST2 r w ()
instanceTest1 = run instanceTestBody1

instanceTestBody1
  :: TestInstrument3 (EmitST2ArrayFxp Int a r w) r w
instanceTestBody1 = testInstrument3

instanceTest2 :: ST2 r w ()
instanceTest2 = run instanceTestBody2

instanceTestBody2
  :: TestInstrument3 (EmitST2ArrayFxp Integer a r w) r w
instanceTestBody2 = testInstrument3
