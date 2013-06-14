-- Copyright 2013 Kevin Backhouse.

{-|
An example of the use of the
'Control.Monad.MultiPass.Instrument.Counter.Counter' instrument.
-}

module Control.Monad.MultiPass.Example.Counter
  ( Tree(..), convertTree )
where

import Control.Monad.ST2
import Control.Monad.MultiPass
import Control.Monad.MultiPass.Instrument.CreateST2Array
import Control.Monad.MultiPass.Instrument.Counter
import Data.Ix

newtype ConvertTree i a r w p1 p2 tc
  = ConvertTree (ConvertTreeType i a r w p1 p2 tc)

type ConvertTreeType i a r w p1 p2 tc
  =  Counter i r w p1 p2 tc
  -> CreateST2Array r w p2 tc
  -> MultiPassMain r w tc (p2 (Tree r w i (i,a)))

instance MultiPassAlgorithm
           (ConvertTree i a r w p1 p2 tc)
           (ConvertTreeType i a r w p1 p2 tc)
           where
  unwrapMultiPassAlgorithm (ConvertTree f) = f

data Tree r w i a
  = Node a (ST2Array r w i (Tree r w i a))

convertTree
  :: (Ix i, Num i)
  => Tree r w i a
  -> ST2 r w (Tree r w i (i,a))
convertTree t =
  run $ PassS $ PassS $ PassZ $ ConvertTree $ convertTreeMP t

convertTreeMP
  :: (Ix i, Num i, Monad p1, Monad p2)
  => Tree r w i a
  -> ConvertTreeType i a r w p1 p2 tc
convertTreeMP t cnt cr =
  mkMultiPassMain
    (return ())
    (\() -> convertSubTree t cnt cr)
    return

convertSubTree
  :: (Ix i, Num i, Monad p1, Monad p2)
  => Tree r w i a
  -> Counter i r w p1 p2 tc
  -> CreateST2Array r w p2 tc
  -> MultiPass r w tc (p2 (Tree r w i (i,a)))
convertSubTree (Node v xs) cnt cr =
  do pk <- postIncr cnt
     -- Use two threads to convert the children.
     pxs <- pmapST2ArrayMP cr (NumThreads 2) xs $ \x ->
              convertSubTree x cnt cr
     return $
       do k <- pk
          xs' <- pxs
          return (Node (k,v) xs')
