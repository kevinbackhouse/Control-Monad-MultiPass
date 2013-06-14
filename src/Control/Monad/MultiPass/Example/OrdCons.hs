-- Copyright 2013 Kevin Backhouse.

{-|
An example of the use of the
'Control.Monad.MultiPass.Instrument.OrdCons.OrdCons' instrument.
-}

module Control.Monad.MultiPass.Example.OrdCons ( convertArray )
where

import Control.Monad.ST2
import Control.Monad.MultiPass
import Control.Monad.MultiPass.Instrument.CreateST2Array
import Control.Monad.MultiPass.Instrument.OrdCons
import Data.Ix

newtype ConvertArray i a r w p1 p2 tc
  = ConvertArray (ConvertArrayType i a r w p1 p2 tc)

type ConvertArrayType i a r w p1 p2 tc
  =  OrdCons a r w p1 p2 tc
  -> CreateST2Array r w p2 tc
  -> MultiPassMain r w tc (p2 (ST2Array r w i Int))

instance MultiPassAlgorithm
           (ConvertArray i a r w p1 p2 tc)
           (ConvertArrayType i a r w p1 p2 tc)
           where
  unwrapMultiPassAlgorithm (ConvertArray f) = f

convertArray
  :: (Ix i, Num i, Ord a)
  => NumThreads
  -> ST2Array r w i a
  -> ST2 r w (ST2Array r w i Int)
convertArray n xs =
  run $ PassS $ PassS $ PassZ $ ConvertArray $
  convertArrayMP n xs

convertArrayMP
  :: (Ix i, Num i, Ord a, Monad p1, Monad p2)
  => NumThreads
  -> ST2Array r w i a
  -> ConvertArrayType i a r w p1 p2 tc
convertArrayMP n xs oc cr =
  mkMultiPassMain
    (return ())
    (\() ->
     pmapST2ArrayMP cr n xs $ \x ->
       ordCons oc (return x))
    return
