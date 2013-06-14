-- Copyright 2013 Kevin Backhouse.

{-|
A variation on the 'Control.Monad.MultiPass.Example.Repmin.repmin'
example. This example shows how the
'Control.Monad.MultiPass.Instrument.Knot3.Knot3' can be used in a
recursive algorithm.
-}

module Control.Monad.MultiPass.Example.Localmin
  ( Tree(..)
  , localmin, localminMP
  )
where

import Control.Monad ( liftM2 )
import Control.Monad.ST2
import Control.Monad.MultiPass
import Control.Monad.MultiPass.Instrument.Knot3

data Tree a
  = Leaf !a
  | Node !(Tree a) !(Tree a)
    deriving (Eq, Show)

-- | Version using lazy evaluation.
localmin :: Ord a => Tree a -> Tree [a]
localmin t =
  snd (localminWalk [] t)

localminWalk :: Ord a => [a] -> Tree a -> (a, Tree [a])
localminWalk ns t =
  case t of
    Leaf n
      -> (n, Leaf (n:ns))

    Node t1 t2
      -> let (n1,tr1) = localminWalk ns' t1
             (n2,tr2) = localminWalk ns' t2
             n = min n1 n2
             ns' = n:ns
         in
         (n, Node tr1 tr2)

type LocalminType r w a p1 p2 p3 tc
  =  Knot3 a r w p1 p2 p3 tc
  -> MultiPassMain r w tc (p3 (Tree [a]))

newtype Localmin r w a p1 p2 p3 tc
  = Localmin (LocalminType r w a p1 p2 p3 tc)

instance MultiPassAlgorithm
           (Localmin r w a p1 p2 p3 tc)
           (LocalminType r w a p1 p2 p3 tc)
           where
  unwrapMultiPassAlgorithm (Localmin f) = f

-- | Version using the "Control.Monad.MultiPass" library.
localminMP :: Ord a => Tree a -> ST2 r w (Tree [a])
localminMP t =
  run (localminTopMP t)

localminTopMP
  :: Ord a
  => Tree a
  -> PassS (PassS (PassS PassZ)) (Localmin r w a)
localminTopMP t =
  PassS $ PassS $ PassS $ PassZ $ Localmin $ \kn ->
  mkMultiPassMain
    (return ())
    (\() -> localminWalkMP kn t (return []))
    (\(_,t') -> return t')

localminWalkMP
  :: (Ord a, Monad p1, Monad p2, Monad p3)
  => Knot3 a r w p1 p2 p3 tc
  -> Tree a
  -> p3 [a]
  -> MultiPass r w tc (p2 a, p3 (Tree [a]))
localminWalkMP kn t ns =
  case t of
    Leaf n
      -> return
           ( return n
           , do ns' <- ns
                return (Leaf (n:ns'))
           )

    Node t1 t2
      -> knot3 kn $ \n ->
         let ns' = liftM2 (:) n ns in
         do (n1,tr1) <- localminWalkMP kn t1 ns'
            (n2,tr2) <- localminWalkMP kn t2 ns'
            let n' = liftM2 min n1 n2
            return (n', (n', liftM2 Node tr1 tr2))
