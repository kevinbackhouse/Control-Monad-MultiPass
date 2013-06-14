-- Copyright 2013 Kevin Backhouse.

{-|
Utility functions for the "Control.Monad.MultiPass" library.
-}

module Control.Monad.MultiPass.Utils
  ( mapST2ArrayMP
  , mapST2ArrayMP_
  , pmapM
  )
where

import Control.Monad.ST2
import Control.Monad.MultiPass
import Data.Ix
import qualified Data.Traversable as T

-- | This function provides a similar interface to
-- 'Control.Monad.mapM', but is specifically for mapping over the
-- 'ST2Array' datatype in the 'Control.Monad.MultiPass.MultiPass'
-- monad.
mapST2ArrayMP
  :: (Ix i, Num i)
  => NumThreads                  -- ^ Number of threads to spawn
  -> ST2Array r w i a            -- ^ Input array
  -> (a -> MultiPass r w tc b)   -- ^ Mapping function
  -> MultiPass r w tc (ST2Array r w i b)  -- ^ Output array
mapST2ArrayMP nThreads xs f =
  let f' i =
        do x <- readOnlyST2ToMP $ readST2Array xs i
           f x
  in
  do bnds <- readOnlyST2ToMP $ boundsST2Array xs
     parallelMP nThreads bnds f'

-- | This function provides a similar interface to
-- 'Control.Monad.mapM_', but is specifically for mapping over the
-- 'ST2Array' datatype in the 'Control.Monad.MultiPass.MultiPass'
-- monad.
mapST2ArrayMP_
  :: (Ix i, Num i)
  => NumThreads                  -- ^ Number of threads to spawn
  -> ST2Array r w i a            -- ^ Input array
  -> (a -> MultiPass r w tc b)   -- ^ Mapping function
  -> MultiPass r w tc ()
mapST2ArrayMP_ nThreads xs f =
  let f' i =
        do x <- readOnlyST2ToMP $ readST2Array xs i
           f x
  in
  do bnds <- readOnlyST2ToMP $ boundsST2Array xs
     parallelMP_ nThreads bnds f'

-- | This function provides a similar interface to
-- 'T.Traversable.mapM', but is useful for mapping over a datatype in
-- a specific pass of the 'Control.Monad.MultiPass.MultiPass' monad.
-- Note: the @m@ type is usually the
-- 'Control.Monad.MultiPass.MultiPass' monad, but the implementation
-- does not specifically depend on anything from the
-- "Control.Monad.MultiPass" library, so its type is more general.
pmapM
  :: (T.Traversable t, Monad m, Monad p)
  => t a
  -> (a -> m (p b))
  -> m (p (t b))
pmapM xs f =
  do xs' <- T.mapM f xs
     return (T.mapM id xs')
