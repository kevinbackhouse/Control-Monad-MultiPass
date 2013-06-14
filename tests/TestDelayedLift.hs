-- Copyright 2013 Kevin Backhouse.

module TestDelayedLift ( instanceTest ) where

import Control.Monad.ST2
import Control.Monad.MultiPass
import Control.Monad.MultiPass.Instrument.DelayedLift
import Control.Monad.MultiPass.Utils.InstanceTest

-- This test checks that all the necessary instances have been
-- defined. Its only purpose is to check that there are no compile
-- errors, so it does not need to be executed.
instanceTest :: ST2 r w ()
instanceTest = run instanceTestBody

instanceTestBody :: TestInstrument1 (DelayedLift r w) r w
instanceTestBody = testInstrument1
