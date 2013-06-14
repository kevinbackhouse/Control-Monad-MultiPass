-- Copyright 2013 Kevin Backhouse.

module TestCFG ( tests ) where

import qualified Test.Framework as TF ( Test )
import Test.Framework.Providers.HUnit

import Control.Monad ( when )
import Control.Monad.ST2
import Control.Monad.MultiPass.Example.CFG
import Data.Array

tests :: [TF.Test]
tests =
  [ testCase "example0" $ prop_emitCFG input0 output0
  , testCase "example1" $ prop_emitCFG input1 output1
  ]

prop_emitCFG :: [[Int]] -> [Int] -> IO ()
prop_emitCFG input output =
  st2ToIO $
  do output' <- runEmitCFG input
     when (output /= output') $ fail "prop_emitCFG"

-- | Simple wrapper around 'emitCFG' which reads the graph from a list
-- and returns the result as a list.
runEmitCFG :: [[Int]] -> ST2 r w [Int]
runEmitCFG g =
  let g' = listArray (Node 0, Node (length g - 1)) $ map (map Node) g in
  do xs <- emitCFG g'
     bnds <- boundsST2Array xs
     sequence
       [ readST2Array xs k
       | k <- range bnds
       ]

input0 :: [[Int]]
input0 =
  [ [0,3]
  , [2]
  , [3]
  , [0,1]
  ]

output0 :: [Int]
output0 =
  [2,2,8,1,3,1,4,2,-5,-4]

input1 :: [[Int]]
input1 =
  [ [0,4]
  , [2]
  , [1,3]
  , [3]
  , [0,1,5]
  , []
  ]

output1 :: [Int]
output1 =
  [2,2,12,1,4,2,-1,3,1,1,3,-8,-7,2,0]
