-- Copyright 2013 Kevin Backhouse.

module TestLocalmin ( tests ) where

import qualified Test.Framework as TF ( Test )
import qualified Test.QuickCheck as QC
import Test.Framework.Providers.QuickCheck2 ( testProperty )

import Control.Monad.ST2
import Control.Monad.MultiPass.Example.Localmin
import Control.Monad.State.Strict
import TestST2

tests :: [TF.Test]
tests =
  [ testProperty "equivalence0" (prop_equivalence (RandomTree example0))
  , testProperty "equivalence1" prop_equivalence
  ]

-- | Run both versions of the algorithm and check that they compute
-- the same result.
prop_equivalence :: RandomTree Int -> TestST2 Bool
prop_equivalence (RandomTree t) =
  TestST2 $ PureST2 $
  let t1 = localmin t in
  do t2 <- localminMP t
     return (t1 == t2)

-- Simple wrapper around the Tree datatype, the only purpose of
-- which is to define a QC.Arbitary instance.
newtype RandomTree a
  = RandomTree (Tree a)

instance Show a => Show (RandomTree a) where
  show (RandomTree t) = show t

instance QC.Arbitrary a => QC.Arbitrary (RandomTree a) where
  arbitrary =
    do budget <- QC.choose (0 :: Int, 256)
       t <- evalStateT randomTree budget
       return (RandomTree t)

-- | The 'StateT' monad is used to track the remaining budget. When it
-- hits zero, only Leaf nodes are generated. This is to prevent
-- excessively large trees from being generated.
randomTree :: QC.Arbitrary a => StateT Int QC.Gen (Tree a)
randomTree =
  do budget <- get
     k <- lift $ QC.choose (0 :: Int, 2)
     if budget == 0 || k == 0
        then do x <- lift QC.arbitrary
                return (Leaf x)
        else do put (budget-1)
                t1 <- randomTree
                t2 <- randomTree
                return (Node t1 t2)

example0 :: Tree Int
example0 =
  Node
    (Node
       (Node
          (Leaf 5)
          (Leaf 2))
       (Leaf 3))
    (Node
       (Leaf 9)
       (Node
          (Node
             (Leaf 4)
             (Leaf 8))
          (Leaf 6)))
