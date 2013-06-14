-- Copyright 2013 Kevin Backhouse.

module TestCounter ( tests, printTree, instanceTest ) where

import qualified Test.Framework as TF ( Test )
import Test.Framework.Providers.QuickCheck2 ( testProperty )

import Control.Monad.ST2
import Control.Monad.State.Strict
import Control.Monad.MultiPass
import Control.Monad.MultiPass.Instrument.Counter
import Control.Monad.MultiPass.Example.Counter
import Control.Monad.MultiPass.Utils.InstanceTest
import Data.Ix
import TestST2

tests :: [TF.Test]
tests =
  [ testProperty "example0" $ prop_convertTree 0
  , testProperty "example1" $ prop_convertTree 1
  , testProperty "example2" $ prop_convertTree 2
  , testProperty "example2" $ prop_convertTree 3
  ]

prop_convertTree :: Int -> TestST2 Bool
prop_convertTree n =
  TestST2 $ PureST2 $
  do t <- mkExample n
     t' <- convertTree t
     evalStateT (checkResult t t') 0

-- Check that the result is correct. This function uses the StateT
-- monad transformer to track the current count.
checkResult
  :: (Eq a, Eq i, Enum i, Ix i)
  => Tree r w i a
  -> Tree r w i (i,a)
  -> StateT i (ST2 r w) Bool
checkResult (Node x ts) (Node (i,x') ts') =
  do i' <- get
     bnds <- lift $ boundsST2Array ts
     bnds' <- lift $ boundsST2Array ts'
     if i == i' && x == x' && bnds == bnds'
        then do put (succ i)
                bs <-
                  sequence
                    [ do t <- lift $ readST2Array ts k
                         t' <- lift $ readST2Array ts' k
                         checkResult t t'
                    | k <- range bnds
                    ]
                return (all id bs)
        else return False

printTree
  :: (Ix i, Enum i, Show a)
  => Int
  -> Tree () w i a
  -> ST2 () () ()
printTree n (Node x ts) =
  do ioToST2 $ putStrLn (replicate n ' ' ++ show x)
     bnds <- boundsST2Array ts
     sequence_
       [ do t <- readST2Array ts i
            printTree (n+2) t
       | i <- range bnds
       ]

-- Generate an example tree of depth n.
mkExample :: Int -> ST2 r w (Tree r w Int Int)
mkExample n =
  do rs <- newST2Array_ (0,n-1)
     when (n > 0) $
       do r <- mkExample (n-1)
          sequence_
            [ writeST2Array rs i r
            | i <- [0 .. n-1]
            ]
     return (Node (n+1) rs)

-- These tests check that all the necessary instances have been
-- defined.
instanceTest :: ST2 r w ()
instanceTest =
  do instanceTest1
     instanceTest2

instanceTest1 :: ST2 r w ()
instanceTest1 = run instanceTestBody1

instanceTestBody1 :: TestInstrument2 (Counter Int r w) r w
instanceTestBody1 = testInstrument2

instanceTest2 :: ST2 r w ()
instanceTest2 = run instanceTestBody2

instanceTestBody2 :: TestInstrument2 (Counter Integer r w) r w
instanceTestBody2 = testInstrument2
