-- Copyright 2013 Kevin Backhouse.

{-|
This example is a variation on the
'Control.Monad.MultiPass.Example.Assembler.assembler' example.  It
illustrates how one might convert a control flow graph into a linear
sequence of instructions. The example is less complete than the
'Control.Monad.MultiPass.Example.Assembler.assembler' example, so the
output is not real machine code. Instead the output is a simple
serialised representation of the control flow graph.

In this example, the control flow graph is represented as a
'Data.Array.Array', which is an immutable datatype. The example can
also be implemented with a mutable representation of the control flow
graph, as shown in "Control.Monad.MultiPass.Example.CFG2".
-}

module Control.Monad.MultiPass.Example.CFG ( Node(..), emitCFG )
where

import Control.Monad.ST2
import Control.Monad.MultiPass
import Control.Monad.MultiPass.Instrument.EmitST2Array
import Control.Monad.MultiPass.Instrument.Knot3
import Control.Monad.MultiPass.Instrument.Delay
import Control.Monad.MultiPass.Utils
import Data.Array

type CFG = Array Node [Node]

newtype Node
  = Node Int
    deriving (Eq, Ord, Ix)

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
  =  Knot3 (Array Node Position) r w p1 p2 p3 tc
  -> EmitST2Array Position Int r w p1 p2 p3 tc
  -> Delay p2 p3 tc
  -> MultiPassMain r w tc (p3 (ST2Array r w Position Int))

newtype EmitCFG r w p1 p2 p3 tc =
  EmitCFG (EmitCFGType r w p1 p2 p3 tc)

instance MultiPassAlgorithm
           (EmitCFG r w p1 p2 p3 tc)
           (EmitCFGType r w p1 p2 p3 tc)
           where
  unwrapMultiPassAlgorithm (EmitCFG f) = f

emitCFG :: CFG -> ST2 r w (ST2Array r w Position Int)
emitCFG g =
  run $ PassS $ PassS $ PassS $ PassZ $ EmitCFG $
  emitMain g

emitMain
  :: (Monad p1, Monad p2, Monad p3)
  => CFG
  -> EmitCFGType r w p1 p2 p3 tc
emitMain g kn emitter delay12 =
  mkMultiPassMain
    (return ())
    (\() -> knot3 kn (emitNodes emitter delay12 g))
    (\() -> getResult emitter)

emitNodes
  :: (Monad p1, Monad p2, Monad p3)
  => EmitST2Array Position Int r w p1 p2 p3 tc
  -> Delay p2 p3 tc
  -> CFG
  -> p3 (Array Node Position)
  -> MultiPass r w tc (p2 (Array Node Position), ())
emitNodes emitter delay12 g offsets =
  do g' <- pmapM g (emitNode emitter delay12 offsets)
     return (g', ())

emitNode
  :: (Monad p1, Monad p2, Monad p3)
  => EmitST2Array Position Int r w p1 p2 p3 tc
  -> Delay p2 p3 tc
  -> p3 (Array Node Position)
  -> [Node]
  -> MultiPass r w tc (p2 Position)
emitNode emitter delay12 offsets ys =
  do -- Emit the number of edges.
     emit emitter (return (length ys))
     sequence_
       [ do -- Emit a relative offset for each edge.
            pos <- getIndex emitter
            emit emitter $
              do pos' <- delay delay12 pos
                 offsets' <- offsets
                 let offset = offsets' ! y
                 return (positionDiff offset pos')
       | y <- ys
       ]
     getIndex emitter

positionDiff :: Position -> Position -> Int
positionDiff (Position a) (Position b) =
  a - b
