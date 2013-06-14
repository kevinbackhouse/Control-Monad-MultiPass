-- Copyright 2013 Kevin Backhouse.

module TestStringInterning ( tests, instanceTest ) where

import qualified Test.Framework as TF ( Test )
import Test.Framework.Providers.QuickCheck2 ( testProperty )

import Control.Monad.ST2
import Control.Monad.MultiPass
import Control.Monad.MultiPass.Instrument.OrdCons
import Control.Monad.MultiPass.Example.StringInterning
import Control.Monad.MultiPass.Utils.InstanceTest
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.Array
import TestST2

tests :: [TF.Test]
tests =
  [ testProperty "equivalence" prop_equivalence
  ]

-- Generate an array with a repeating sequence of strings.
-- For example:
--
--    ["a","b","c","a","b","c","a","b","c","a","b","c"]
--
-- Then check that convertArray correctly classifies the numbers
-- in the array:
--
--    [0,1,2,0,1,2,0,1,2,0,1,2]
--
prop_equivalence :: Int -> TestST2 Bool
prop_equivalence n0 =
  let strs = listArray (0,2) ["cat","dog","rabbit"] in
  let n = n0 `mod` 256 in
  TestST2 $ PureST2 $
  do xs <- newST2Array_ (0,n-1)
     sequence_
       [ writeST2Array xs i (strs ! (i `mod` 3))
       | i <- [0 .. n-1]
       ]
     (ys,_) <- internStringArray (NumThreads 4) xs
     All success <-
       execWriterT $
       sequence_
       [ do x <- lift $ readST2Array xs i
            y <- lift $ readST2Array ys i
            tell $ All $ x == strs ! y
       | i <- [0 .. n-1]
       ]
     return success

-- This test checks that all the necessary instances have been
-- defined. Its only purpose is to check that there are no compile
-- errors, so it does not need to be executed.
instanceTest :: ST2 r w ()
instanceTest = run instanceTestBody

instanceTestBody
  :: TestInstrument2 (OrdCons String r w) r w
instanceTestBody = testInstrument2
