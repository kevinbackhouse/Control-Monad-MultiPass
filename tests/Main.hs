-- Copyright 2013 Kevin Backhouse.

module Main where

import Test.Framework as TF ( defaultMain, testGroup, Test )
import qualified TestAssembler
import qualified TestCFG
import qualified TestCFG2
import qualified TestCounter
import qualified TestLocalmin
import qualified TestOrdCons
import qualified TestRepmin
import qualified TestStringInterning

-- These modules currently only contain instance tests:
import TestCreateST2Array ()
import TestDelay ()
import TestDelayedLift ()
import TestEmitST2Array ()
import TestEmitST2ArrayFxp ()
import TestKnot3 ()
import TestMonoid2 ()
import TestTopKnot ()

main :: IO ()
main = defaultMain tests

tests :: [TF.Test]
tests =
  [ testGroup "Assembler" TestAssembler.tests
  , testGroup "CFG" TestCFG.tests
  , testGroup "CFG2" TestCFG2.tests
  , testGroup "Counter" TestCounter.tests
  , testGroup "Localmin" TestLocalmin.tests
  , testGroup "OrdCons" TestOrdCons.tests
  , testGroup "Repmin" TestRepmin.tests
  , testGroup "TestStringInterning" TestStringInterning.tests
  ]
