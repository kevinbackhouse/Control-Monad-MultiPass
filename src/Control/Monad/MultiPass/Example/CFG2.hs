-- Copyright 2013 Kevin Backhouse.

{-|
This example is a modified version of the
"Control.Monad.MultiPass.Example.CFG" example, which uses a mutable
'ST2Array' to represent the control flow graph rather than an
immutable 'Data.Array.Array'. This means that it is not possible to
use 'Control.Monad.MultiPass.Utils.pmapM' to map over the array.
Instead 'pmapST2ArrayMP' is used
-}

module Control.Monad.MultiPass.Example.CFG2 ( Node(..), emitCFG )
where

import Control.Monad.ST2
import Control.Monad.MultiPass
import Control.Monad.MultiPass.Instrument.EmitST2Array
import Control.Monad.MultiPass.Instrument.Knot3
import Control.Monad.MultiPass.Instrument.Delay
import Control.Monad.MultiPass.Instrument.CreateST2Array
import Control.Monad.MultiPass.Instrument.DelayedLift
import Data.Ix

type CFG r w = ST2Array r w Node [Node]

newtype Node
  = Node Int
    deriving (Eq, Ord, Ix)

instance Num Node where
  (Node x) + (Node y) = Node (x + y)
  (Node x) - (Node y) = Node (x - y)
  (Node x) * (Node y) = Node (x * y)
  negate (Node x) = Node (negate x)
  abs (Node x) = Node (abs x)
  signum (Node x) = Node (signum x)
  fromInteger x = Node (fromInteger x)

newtype Position
  = Position Int
    deriving (Eq, Ord, Ix)

instance Num Position where
  (Position x) + (Position y) = Position (x + y)
  (Position x) - (Position y) = Position (x - y)
  (Position x) * (Position y) = Position (x * y)
  negate (Position x) = Position (negate x)
  abs (Position x) = Position (abs x)
  signum (Position x) = Position (signum x)
  fromInteger x = Position (fromInteger x)

type EmitCFGType r w p1 p2 p3 tc
  =  Knot3 (ST2Array r w Node Position) r w p1 p2 p3 tc
  -> EmitST2Array Position Int r w p1 p2 p3 tc
  -> Delay p2 p3 tc
  -> DelayedLift r w p3 tc
  -> CreateST2Array r w p2 tc
  -> MultiPassMain r w tc (p3 (ST2Array r w Position Int))

newtype EmitCFG r w p1 p2 p3 tc =
  EmitCFG (EmitCFGType r w p1 p2 p3 tc)

instance MultiPassAlgorithm
           (EmitCFG r w p1 p2 p3 tc)
           (EmitCFGType r w p1 p2 p3 tc)
           where
  unwrapMultiPassAlgorithm (EmitCFG f) = f

emitCFG :: NumThreads -> CFG r w -> ST2 r w (ST2Array r w Position Int)
emitCFG n g =
  run $ PassS $ PassS $ PassS $ PassZ $ EmitCFG $
  emitMain n g

emitMain
  :: (Monad p1, Monad p2, Monad p3)
  => NumThreads
  -> CFG r w
  -> EmitCFGType r w p1 p2 p3 tc
emitMain n g kn emitter delay12 dlift cr =
  mkMultiPassMain
    (return ())
    (\() -> knot3 kn (emitNodes n emitter delay12 dlift cr g))
    (\() -> getResult emitter)

emitNodes
  :: (Monad p1, Monad p2, Monad p3)
  => NumThreads
  -> EmitST2Array Position Int r w p1 p2 p3 tc
  -> Delay p2 p3 tc
  -> DelayedLift r w p3 tc
  -> CreateST2Array r w p2 tc
  -> CFG r w
  -> p3 (ST2Array r w Node Position)
  -> MultiPass r w tc (p2 (ST2Array r w Node Position), ())
emitNodes n emitter delay12 dlift cr g offsets =
  do g' <- pmapST2ArrayMP cr n g (emitNode emitter delay12 dlift offsets)
     return (g', ())

emitNode
  :: (Monad p1, Monad p2, Monad p3)
  => EmitST2Array Position Int r w p1 p2 p3 tc
  -> Delay p2 p3 tc
  -> DelayedLift r w p3 tc
  -> p3 (ST2Array r w Node Position)
  -> [Node]
  -> MultiPass r w tc (p2 Position)
emitNode emitter delay12 dlift offsets ys =
  do emit emitter (return (length ys))
     sequence_
       [ do pos <- getIndex emitter
            offset <- readST2ArrayMP dlift offsets y
            emit emitter $
              do pos' <- delay delay12 pos
                 offset' <- offset
                 return (positionDiff offset' pos')
       | y <- ys
       ]
     getIndex emitter

positionDiff :: Position -> Position -> Int
positionDiff (Position a) (Position b) =
  a - b
