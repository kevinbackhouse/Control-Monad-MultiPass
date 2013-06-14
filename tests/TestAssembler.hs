-- Copyright 2013 Kevin Backhouse.

module TestAssembler ( tests ) where

import qualified Test.Framework as TF ( Test )
import Test.Framework.Providers.HUnit

import Control.Monad ( when )
import Control.Monad.ST2
import Control.Monad.MultiPass
import Control.Monad.MultiPass.Example.Assembler
import Data.Array
import Data.Word

tests :: [TF.Test]
tests =
  concat $
  [ [ testCase "example1" $ prop_assemble n (genExample 1) output1
    , testCase "example2" $ prop_assemble n (genExample 2) output2
    , testCase "example3" $ prop_assemble n (genExample 3) output3
    , testCase "example10" $ prop_assemble n (genExample 10) output10
    , testCase "example30" $ prop_assemble n (genExample 30) output30
    ]
  | n <- [1..4]
  ]

prop_assemble :: Int -> [Instruction] -> [Word8] -> IO ()
prop_assemble nThreads input output =
  st2ToIO $
  do output' <- runAssembler nThreads input
     when (output /= output') $ fail (show output')

-- | Simple wrapper around 'assembler' which reads the instructions
-- from a list and returns the result as a list.
runAssembler :: Int -> [Instruction] -> ST2 r w [Word8]
runAssembler nThreads example =
  do instructions <- newST2Array_ (0, length example - 1)
     sequence_
       [ writeST2Array instructions i instr
       | (i,instr) <- zip [0..] example
       ]
     xs <- assemble (NumThreads nThreads) instructions
     bnds <- boundsST2Array xs
     sequence
       [ readST2Array xs i
       | i <- range bnds
       ]

output1 :: [Word8]
output1 =
  [ 72, 131, 192, 0, 235, 250, 72, 131, 192, 0 ]

output2 :: [Word8]
output2 =
  [ 72, 131, 192, 0, 72, 131, 192, 1, 235, 250, 72, 131, 192, 0, 235,
    240, 72, 131, 192, 1 ]

output3 :: [Word8]
output3 =
  [ 72, 131, 192, 0, 72, 131, 192, 1, 72, 131, 192, 2, 235, 250, 72,
    131, 192, 0, 235, 240, 72, 131, 192, 1, 235, 230, 72, 131, 192, 2 ]

output10 :: [Word8]
output10 =
  [ 72, 131, 192, 0, 72, 131, 192, 1, 72, 131, 192, 2, 72, 131, 192,
    3, 72, 131, 192, 4, 72, 131, 192, 5, 72, 131, 192, 6, 72, 131,
    192, 7, 72, 131, 192, 8, 72, 131, 192, 9, 235, 250, 72, 131, 192,
    0, 235, 240, 72, 131, 192, 1, 235, 230, 72, 131, 192, 2, 235, 220,
    72, 131, 192, 3, 235, 210, 72, 131, 192, 4, 235, 200, 72, 131,
    192, 5, 235, 190, 72, 131, 192, 6, 235, 180, 72, 131, 192, 7, 235,
    170, 72, 131, 192, 8, 235, 160, 72, 131, 192, 9 ]

output30 :: [Word8]
output30 =
  [ 72, 131, 192, 0, 72, 131, 192, 1, 72, 131, 192, 2, 72, 131, 192,
    3, 72, 131, 192, 4, 72, 131, 192, 5, 72, 131, 192, 6, 72, 131,
    192, 7, 72, 131, 192, 8, 72, 131, 192, 9, 72, 131, 192, 10, 72,
    131, 192, 11, 72, 131, 192, 12, 72, 131, 192, 13, 72, 131, 192,
    14, 72, 131, 192, 15, 72, 131, 192, 16, 72, 131, 192, 17, 72, 131,
    192, 18, 72, 131, 192, 19, 72, 131, 192, 20, 72, 131, 192, 21, 72,
    131, 192, 22, 72, 131, 192, 23, 72, 131, 192, 24, 72, 131, 192,
    25, 72, 131, 192, 26, 72, 131, 192, 27, 72, 131, 192, 28, 72, 131,
    192, 29, 235, 250, 72, 131, 192, 0, 235, 240, 72, 131, 192, 1,
    235, 230, 72, 131, 192, 2, 235, 220, 72, 131, 192, 3, 235, 210,
    72, 131, 192, 4, 235, 200, 72, 131, 192, 5, 235, 190, 72, 131,
    192, 6, 235, 180, 72, 131, 192, 7, 235, 170, 72, 131, 192, 8, 235,
    160, 72, 131, 192, 9, 235, 150, 72, 131, 192, 10, 235, 140, 72,
    131, 192, 11, 235, 130, 72, 131, 192, 12, 233, 117, 255, 255, 255,
    72, 131, 192, 13, 233, 104, 255, 255, 255, 72, 131, 192, 14, 233,
    91, 255, 255, 255, 72, 131, 192, 15, 233, 78, 255, 255, 255, 72,
    131, 192, 16, 233, 65, 255, 255, 255, 72, 131, 192, 17, 233, 52,
    255, 255, 255, 72, 131, 192, 18, 233, 39, 255, 255, 255, 72, 131,
    192, 19, 233, 26, 255, 255, 255, 72, 131, 192, 20, 233, 13, 255,
    255, 255, 72, 131, 192, 21, 233, 0, 255, 255, 255, 72, 131, 192,
    22, 233, 243, 254, 255, 255, 72, 131, 192, 23, 233, 230, 254, 255,
    255, 72, 131, 192, 24, 233, 217, 254, 255, 255, 72, 131, 192, 25,
    233, 204, 254, 255, 255, 72, 131, 192, 26, 233, 191, 254, 255,
    255, 72, 131, 192, 27, 233, 178, 254, 255, 255, 72, 131, 192, 28,
    233, 165, 254, 255, 255, 72, 131, 192, 29 ]

-- Generate an example input containing some add instructions, labels,
-- and gotos. The argument n determines the size of the example
-- generated.
genExample :: Int -> [Instruction]
genExample n =
  concat
    [ [ Label $ LabelName $ show i
      , AddImm8 (Register 0) (fromIntegral i)
      ]
    | i <- [0 .. n-1]
    ] ++
  concat
    [ [ Goto $ LabelName $ show (n - i - 1)
      , AddImm8 (Register 0) (fromIntegral i)
      ]
    | i <- [0 .. n-1]
    ]
