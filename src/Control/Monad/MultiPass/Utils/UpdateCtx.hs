-- Copyright 2013 Kevin Backhouse.

{-|
Utility functions for working with the 'UpdateThreadContext'
argument of 'createInstrument'. This module is only relevant for
Instrument authoring.
-}

module Control.Monad.MultiPass.Utils.UpdateCtx
  ( updateCtxFst, updateCtxSnd
  , updateCtxLeft, updateCtxRight
  )
where

import Control.Exception ( assert )
import Control.Monad.MultiPass

-- | If the thread context is a pair then 'updateCtxFst' creates a new
-- 'UpdateThreadContext' function which can be used to update the
-- first element of the pair.
updateCtxFst
  :: UpdateThreadContext rootTC (x,y)
  -> UpdateThreadContext rootTC x
updateCtxFst updateCtx f =
  do (x,_) <- updateCtx (cross f id)
     return x

-- | If the thread context is a pair then 'updateCtxSnd' creates a new
-- 'UpdateThreadContext' function which can be used to update the
-- second element of the pair.
updateCtxSnd
  :: UpdateThreadContext rootTC (x,y)
  -> UpdateThreadContext rootTC y
updateCtxSnd updateCtx f =
  do (_,y) <- updateCtx (cross id f)
     return y

cross :: (a -> a') -> (b -> b') -> (a,b) -> (a',b')
cross f g (x,y) = (f x, g y)

-- | If the thread context is an Either of two thread contexts then
-- 'updateCtxLeft' creates a new 'UpdateThreadContext' function which
-- can be used to update the 'Left' element. This function will assert
-- if the thread context is a 'Right' element.
updateCtxLeft
  :: UpdateThreadContext rootTC (Either x y)
  -> UpdateThreadContext rootTC x
updateCtxLeft updateCtx f =
  let g (Left x) = Left (f x)
      g (Right _) = assert False $ error "updateCtxLeft"
  in
  do Left x <- updateCtx g
     return x

-- | If the thread context is an Either of two thread contexts then
-- 'updateCtxRight' creates a new 'UpdateThreadContext' function which
-- can be used to update the 'Right' element. This function will assert
-- if the thread context is a 'Left' element.
updateCtxRight
  :: UpdateThreadContext rootTC (Either x y)
  -> UpdateThreadContext rootTC y
updateCtxRight updateCtx f =
  let g (Left _) = assert False $ error "updateCtxRight"
      g (Right x) = Right (f x)
  in
  do Right x <- updateCtx g
     return x
