-- Copyright 2013 Kevin Backhouse.

{-|
An example of the use of the
'Control.Monad.MultiPass.Instrument.OrdCons.OrdCons' instrument.
An array of strings is converted to an array of integer indices,
with one index for each distinct string. This process is commonly
known as "string interning".
-}

module Control.Monad.MultiPass.Example.StringInterning
  ( internStringArray )
where

import Control.Monad ( liftM2 )
import Control.Monad.ST2
import Control.Monad.MultiPass
import Control.Monad.MultiPass.Instrument.CreateST2Array
import Control.Monad.MultiPass.Instrument.OrdCons

newtype InternArray r w p1 p2 tc
  = InternArray (InternArrayType r w p1 p2 tc)

type InternArrayType r w p1 p2 tc
  =  OrdCons String r w p1 p2 tc
  -> CreateST2Array r w p2 tc
  -> MultiPassMain r w tc
       (p2 (ST2Array r w Int Int, OrdConsTable String))

instance MultiPassAlgorithm
           (InternArray r w p1 p2 tc)
           (InternArrayType r w p1 p2 tc)
           where
  unwrapMultiPassAlgorithm (InternArray f) = f

internStringArray
  :: NumThreads
  -> ST2Array r w Int String
  -> ST2 r w (ST2Array r w Int Int, OrdConsTable String)
internStringArray n xs =
  run $ PassS $ PassS $ PassZ $ InternArray $ \pool cr ->
  mkMultiPassMain
    (return ())
    (\() -> internStringArrayElems pool cr n xs)
    (\xs' ->
     do table <- getOrdConsTable pool
        return (liftM2 (,) xs' table))

internStringArrayElems
  :: (Monad p1, Monad p2)
  => OrdCons String r w p1 p2 tc
  -> CreateST2Array r w p2 tc
  -> NumThreads
  -> ST2Array r w Int String
  -> MultiPass r w tc (p2 (ST2Array r w Int Int))
internStringArrayElems pool cr n xs =
  pmapST2ArrayMP cr n xs $ \x ->
    ordCons pool (return x)
