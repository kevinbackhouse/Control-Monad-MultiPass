-- Copyright 2013 Kevin Backhouse.

module TestDelay ( instanceTest ) where

import Control.Monad.ST2
import Control.Monad.MultiPass
import Control.Monad.MultiPass.Instrument.Delay
import Control.Monad.MultiPass.Utils.InstanceTest

-- This test checks that all the necessary instances have been
-- defined. Its only purpose is to check that there are no compile
-- errors, so it does not need to be executed.
instanceTest :: ST2 r w ()
instanceTest = run instanceTestBody

instanceTestBody :: TestInstrument2 Delay r w
instanceTestBody = testInstrument2
