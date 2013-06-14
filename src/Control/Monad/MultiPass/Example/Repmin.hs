-- Copyright 2013 Kevin Backhouse.

{-|
An implementation of the classic @repmin@ algorithm, using the
"Control.Monad.MultiPass" library.
-}

module Control.Monad.MultiPass.Example.Repmin
  ( Tree(..)
  , repmin, repminMP, repminMP2, repminMP3
  )
where

import Control.Monad ( liftM, liftM2 )
import Control.Monad.ST2
import Control.Monad.MultiPass
import Control.Monad.MultiPass.Instrument.Knot3
import Control.Monad.MultiPass.Instrument.Monoid2
import Control.Monad.MultiPass.Instrument.TopKnot
import Data.Monoid

-- | Binary tree datatype.
data Tree a
  = Leaf !a
  | Node !(Tree a) !(Tree a)
    deriving (Eq, Show)

-- | Original algorithm, which uses lazy evaluation.
repmin :: Ord a => Tree a -> Tree a
repmin t =
  let (minVal,tr) = repminWalk minVal t in
  tr

repminWalk :: Ord a => b -> Tree a -> (a, Tree b)
repminWalk minVal t =
  case t of
    Leaf n
      -> (n, Leaf minVal)

    Node t1 t2
      -> let (n1,tr1) = repminWalk minVal t1 in
         let (n2,tr2) = repminWalk minVal t2 in
         (min n1 n2, Node tr1 tr2)

type RepminType r w a p1 p2 tc
  =  TopKnot a r w p1 p2 tc
  -> MultiPassMain r w tc (p2 (Tree a))

newtype Repmin r w a p1 p2 tc
  = Repmin (RepminType r w a p1 p2 tc)

instance MultiPassAlgorithm
           (Repmin r w a p1 p2 tc)
           (RepminType r w a p1 p2 tc)
           where
  unwrapMultiPassAlgorithm (Repmin f) = f

-- | New algorithm, using the "Control.Monad.MultiPass" library.
repminMP :: Ord a => Tree a -> ST2 r w (Tree a)
repminMP t =
  run $ PassS $ PassS $ PassZ $ Repmin $ \kn ->
  mkMultiPassMain
    (load kn)
    (repminWalkMP t)
    (\(minVal,t') ->
     do store kn minVal
        return t')

type RepminType2 r w a p1 p2 p3 tc
  =  Knot3 a r w p1 p2 p3 tc
  -> MultiPassMain r w tc (p3 (Tree a))

newtype Repmin2 r w a p1 p2 p3 tc
  = Repmin2 (RepminType2 r w a p1 p2 p3 tc)

instance MultiPassAlgorithm
           (Repmin2 r w a p1 p2 p3 tc)
           (RepminType2 r w a p1 p2 p3 tc)
           where
  unwrapMultiPassAlgorithm (Repmin2 f) = f

-- | Second version of the new algorithm ('repminMP'), using the
-- 'Knot3' instrument, rather than 'TopKnot'.
repminMP2 :: Ord a => Tree a -> ST2 r w (Tree a)
repminMP2 t =
  run $ PassS $ PassS $ PassS $ PassZ $ Repmin2 $ \kn ->
  mkMultiPassMain
    (return ())
    (\() -> knot3 kn (repminWalkMP t))
    return

repminWalkMP
  :: (Ord a, Monad p1, Monad p2)
  => Tree a
  -> p2 a
  -> MultiPass r w tc (p1 a, p2 (Tree a))
repminWalkMP t minVal =
  case t of
    Leaf n
      -> return (return n, liftM Leaf minVal)

    Node t1 t2
      -> do (n1,tr1) <- repminWalkMP t1 minVal
            (n2,tr2) <- repminWalkMP t2 minVal
            return (liftM2 min n1 n2, liftM2 Node tr1 tr2)

type RepminType3 r w a p1 p2 tc
  =  Monoid2 (MinVal a) r w p1 p2 tc
  -> MultiPassMain r w tc (p2 (Tree a))

newtype Repmin3 r w a p1 p2 tc
  = Repmin3 (RepminType3 r w a p1 p2 tc)

instance MultiPassAlgorithm
           (Repmin3 r w a p1 p2 tc)
           (RepminType3 r w a p1 p2 tc)
           where
  unwrapMultiPassAlgorithm (Repmin3 f) = f

-- | Third version of the new algorithm ('repminMP'), using the
-- 'Monoid2' instrument.
repminMP3 :: Ord a => Tree a -> ST2 r w (Tree a)
repminMP3 t =
  run $ PassS $ PassS $ PassZ $ Repmin3 $ \mv ->
  mkMultiPassMain
    (return ())
    (\() -> repminWalkMP3 mv t)
    return

-- The purpose of this type is to define a Monoid instance with
-- min as the mappend method.
data MinVal a
  = Infinity
  | MinVal { getMinVal :: !a }

instance Ord a => Monoid (MinVal a) where
  mempty = Infinity

  mappend x Infinity = x
  mappend Infinity y = y
  mappend (MinVal x) (MinVal y) = MinVal (min x y)

repminWalkMP3
  :: (Ord a, Monad p1, Monad p2)
  => Monoid2 (MinVal a) r w p1 p2 tc
  -> Tree a
  -> MultiPass r w tc (p2 (Tree a))
repminWalkMP3 mv t =
  case t of
    Leaf n
      -> do tell mv (return (MinVal n))
            minVal <- listen mv
            return (liftM (Leaf . getMinVal) minVal)

    Node t1 t2
      -> do tr1 <- repminWalkMP3 mv t1
            tr2 <- repminWalkMP3 mv t2
            return (liftM2 Node tr1 tr2)
