-- Copyright 2013 Kevin Backhouse.

module TestST2 ( TestST2(..) ) where

import Test.QuickCheck as QC
import Test.QuickCheck.Property ( morallyDubiousIOProperty )
import Control.Monad.ST2

-- | 'TestST2' is a trivial wrapper around 'PureST2'. Its only purpose
-- is to define a 'QC.Testable' instance for 'PureST2'.
newtype TestST2 a
  = TestST2 (PureST2 a)

instance QC.Testable a => QC.Testable (TestST2 a) where
  property (TestST2 (PureST2 m)) = morallyDubiousIOProperty (st2ToIO m)
