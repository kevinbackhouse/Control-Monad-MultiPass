-- Copyright 2013 Kevin Backhouse.

{-# OPTIONS_GHC -XPolyKinds -XKindSignatures -XScopedTypeVariables #-}

{-|

This module implements the core functions, datatypes, and classes of
the MultiPass library. Its export list is divided into two halves. The
first half contains the declarations which are relevant to anyone who
wants to use the MultiPass library. The second contains which are only
relevant to people who want to implement new instruments.

-}

module Control.Monad.MultiPass
  ( -- * Users
    MultiPass
  , MultiPassPrologue
  , MultiPassEpilogue
  , MultiPassMain, mkMultiPassMain
  , PassS(..), PassZ(..)
  , MultiPassAlgorithm(..)
  , run
  , NumThreads(..)
  , parallelMP, parallelMP_
  , readOnlyST2ToMP

    -- * Instrument Authors
  , On(..), Off(..)
  , MultiPassBase
  , mkMultiPass, mkMultiPassPrologue, mkMultiPassEpilogue
  , WrapInstrument, wrapInstrument
  , PassNumber
  , StepDirection(..)
  , ST2ToMP
  , UpdateThreadContext
  , Instrument(..)
  , ThreadContext(..)
  , NextThreadContext(..)
  , NextGlobalContext(..)
  , BackTrack(..)
  )
where

import Control.Exception ( assert )
import Control.Monad.State.Strict
import Control.Monad.ST2
import Data.Ix

-- | This datatype is used in conjunction with 'PassZ' to package the
-- main function of the multi-pass algorithm. For an example of how
-- they are used, see the implementation of
-- 'Control.Monad.MultiPass.Example.Repmin.repminMP' or any of the
-- other examples in the Example directory.
newtype PassS cont m
  = PassS (forall p. Monad p => cont (m p))

-- | Used in conjunction with 'PassS' to build a Peano number
-- corresponding to the number of passes.
newtype PassZ f
  = PassZ (forall (tc :: *). f tc)

-- | The main function of a multi-pass algorithm needs to be wrapped
-- in a newtype so that it can be packaged with 'PassS' and
-- 'PassZ'. The newtype needs to be made an instance of
-- 'MultiPassAlgorithm' so that it can unwrapped by the
-- implementation.
class MultiPassAlgorithm a b | a -> b where
  unwrapMultiPassAlgorithm :: a -> b

-- | Trivial monad, equivalent to 'Data.Functor.Identity.Identity'.
-- Used to switch on a pass of a multi-pass algorithm.
newtype On a = On a deriving Functor

instance Monad On where
  return x = On x
  On x >>= f = f x

-- | Trivial monad which computes absolutely nothing. It is used to
-- switch off a pass of a multi-pass algorithm.
data Off (a :: *) = Off deriving Functor

instance Monad Off where
  return _ = Off
  Off >>= _ = Off

-- ArgCons and ArgNil are used to uncurry the main function of the
-- multi-pass algorithm. For example, a function of the following
-- type:
--
--     Instrument1 -> Instrument2 -> MultiPass r w tc a
--
-- gets converted to a function of type:
--
--     ArgCons Instrument1 (ArgCons Instrument2 ArgNil) ->
--     MultiPass r w tc a
--
-- The uncurrying is implemented in the ApplyArg and ApplyArgs
-- classes.
--
-- ArgCons and ArgNil are not exported from this module.
data ArgCons a b
  = ArgCons !a !b

data ArgNil
  = ArgNil

mapArgCons :: (a -> a') -> (b -> b') -> (ArgCons a b) -> (ArgCons a' b')
mapArgCons f g (ArgCons x y) =
  ArgCons (f x) (g y)

-- The Param type is the old solution to the problem of passing
-- initial parameters to instruments. The MultiPassPrologue seems to
-- be a better solution to this problem, so the Param type has been
-- removed from the external interface. However, all the internal
-- plumbing is still there (in ApplyArg and ApplyArgs), so it would be
-- easy to resurrect if necessary. The comments below are the old
-- comments explaining how to use Param.
--
-- This type is used by instruments that are parameterised by an
-- initial value. It is used in the main function of the algorithm as
-- follows:
--
--    mainFcn =
--      Param initVal1 $ \instr1 ->
--      Param initVal2 $ \instr2 ->
--      do ...
--
-- The initial values are passed to the createInstrument method of the
-- Instrument class so that they can be used during the construction
-- of the instrument. This is implemented in the ApplyArg and
-- ApplyArgs classes.
data Param i f
  = Param !i !f

-- | This datatype is used by the 'NextThreadContext' and
-- 'NextGlobalContext' classes to specify whether the algorithm is
-- progressing to the next pass or back-tracking to a previous
-- pass. When back-tracking occurs, the current thread and global
-- contexts are first passed the 'StepReset' command. Then they are
-- passed the 'StepBackward' command @N@ times, where @N@ is the
-- number of passes that need to be revisited. Note that @N@ can be
-- zero if only the current pass needs to be revisited, so the
-- 'StepBackward' command may not be used. This is the reason why the
-- 'StepReset' command is always issued first.
data StepDirection
  = StepForward
  | StepReset
  | StepBackward
    deriving Eq

-- | This datatype is used by the back-tracking mechanism. Instruments
-- can request that the evaluator back-tracks to a specific pass
-- number. Instruments which use back-tracking store the relevant
-- PassNumbers in their global context. The current 'PassNumber' is
-- the first argument of 'nextGlobalContext' for this
-- purpose. 'PassNumber' is an abstract datatype. Instruments should
-- never need to create a new 'PassNumber' or modify an existing one,
-- so no functions that operate on 'PassNumber' are exported from this
-- module.
newtype PassNumber = PassNumber { unwrapPassNumber :: Int }

-- Increment a PassNumber. This function is not exported.
incrPassNumber :: PassNumber -> PassNumber
incrPassNumber (PassNumber k) =
  PassNumber (k+1)

-- Compute the minimum of two PassNumbers. This function is not
-- exported.
minPassNumber :: PassNumber -> PassNumber -> PassNumber
minPassNumber (PassNumber x) (PassNumber y) =
  PassNumber (min x y)

-- | 'MultiPass', 'MultiPassPrologue', and 'MultiPassEpilogue' are
-- trivial newtype wrappers around this monad. Instruments can
-- construct computations in the 'MultiPassBase' monad, but then use
-- 'mkMultiPass', 'mkMultiPassPrologue', and 'mkMultiPassEpilogue' to
-- restrict which of the three stages it is allowed to be used in.
newtype MultiPassBase r w tc a
  = MultiPassBase
      { unwrapMultiPassBase
          :: ThreadContext r w tc => StateT tc (ST2 r w) a
      }
    deriving Functor

instance Monad (MultiPassBase r w tc) where
  return x = MultiPassBase $ return x

  MultiPassBase m >>= f =
    MultiPassBase $
    do x <- m
       unwrapMultiPassBase (f x)

-- | This monad is used to implement the body of a multi-pass
-- algorithm.
newtype MultiPass r w tc a
  = MultiPass
      { unwrapMultiPass :: MultiPassBase r w tc a
      }
    deriving Functor

instance Monad (MultiPass r w tc) where
  return x = MultiPass $ return x

  MultiPass m >>= f =
    MultiPass $
    do x <- m
       unwrapMultiPass (f x)

-- | Restrict a computation so that it can only be executed during the
-- body of the algorithm (not the prologue or epilogue).
mkMultiPass :: MultiPassBase r w tc a -> MultiPass r w tc a
mkMultiPass =
  MultiPass

-- | This monad is used to implement the prologue of a multi-pass
-- algorithm.
newtype MultiPassPrologue r w tc a
  = MultiPassPrologue
      { unwrapMultiPassPrologue :: MultiPassBase r w tc a
      }
    deriving Functor

instance Monad (MultiPassPrologue r w tc) where
  return x = MultiPassPrologue $ return x

  MultiPassPrologue m >>= f =
    MultiPassPrologue $
    do x <- m
       unwrapMultiPassPrologue (f x)

-- | Restrict a computation so that it can only be executed during the
-- prologue.
mkMultiPassPrologue
  :: MultiPassBase r w tc a -> MultiPassPrologue r w tc a
mkMultiPassPrologue =
  MultiPassPrologue

-- | This monad is used to implement the epilogue of a multi-pass
-- algorithm.
newtype MultiPassEpilogue r w tc a
  = MultiPassEpilogue
      { unwrapMultiPassEpilogue :: MultiPassBase r w tc a
      }
    deriving Functor

instance Monad (MultiPassEpilogue r w tc) where
  return x = MultiPassEpilogue $ return x

  MultiPassEpilogue m >>= f =
    MultiPassEpilogue $
    do x <- m
       unwrapMultiPassEpilogue (f x)

-- | Restrict a computation so that it can only be executed during the
-- epilogue.
mkMultiPassEpilogue
  :: MultiPassBase r w tc a -> MultiPassEpilogue r w tc a
mkMultiPassEpilogue =
  MultiPassEpilogue

-- | 'MultiPassMain' is an abstract datatype containing the prologue,
-- body, and epilogue of a multi-pass algorithm. Use
-- 'mkMultiPassMain' to construct an object of type 'MultiPassMain'.
data MultiPassMain r w tc c =
  forall a b.
  MultiPassMain
    !(MultiPassPrologue r w tc a)
    !(a -> MultiPass r w tc b)
    !(b -> MultiPassEpilogue r w tc c)

-- | Combine the prologue, body, and epilogue of a multi-pass
-- algorithm to create the 'MultiPassMain' object which is required by
-- the 'run' function.
mkMultiPassMain
  :: MultiPassPrologue r w tc a           -- ^ Prologue
  -> (a -> MultiPass r w tc b)            -- ^ Algorithm body
  -> (b -> MultiPassEpilogue r w tc c)    -- ^ Epilogue
  -> MultiPassMain r w tc c
mkMultiPassMain prologue body epilogue =
  MultiPassMain prologue body epilogue

-- Run the prologue, body, and epilogue of a multi-pass algorithm.
runMultiPassMain
  :: ThreadContext r w tc
  => MultiPassMain r w tc a
  -> tc
  -> ST2 r w (a, tc)
runMultiPassMain (MultiPassMain prologue body epilogue) =
  runStateT $
  do x <- unwrapMultiPassBase $ unwrapMultiPassPrologue $ prologue
     y <- unwrapMultiPassBase $ unwrapMultiPass $ body x
     unwrapMultiPassBase $ unwrapMultiPassEpilogue $ epilogue y

-- | This class is used when multiple threads are
-- spawned. 'splitThreadContext' is used to create a new thread
-- context for each of the new threads and 'mergeThreadContext' is
-- used to merge them back together when the parallel region ends.
class ThreadContext r w tc where
  splitThreadContext
    :: Int                 -- Number of threads being created
    -> Int                 -- Index of current thread
    -> tc                  -- Current thread context
    -> ST2 r w tc          -- New sub-context

  mergeThreadContext
    :: Int                 -- Number of threads being merged
    -> (Int -> ST2 r w tc) -- Function to get the i'th sub-context
    -> tc                  -- Previous merged context
    -> ST2 r w tc          -- New merged context

instance ThreadContext r w () where
  splitThreadContext _ _ () = return ()
  mergeThreadContext _ _ () = return ()

instance ThreadContext r w ArgNil where
  splitThreadContext _ _ ArgNil = return ArgNil
  mergeThreadContext _ _ ArgNil = return ArgNil

instance (ThreadContext r w x, ThreadContext r w y) =>
         ThreadContext r w (ArgCons x y) where
  splitThreadContext m t (ArgCons x y) =
    do x' <- splitThreadContext m t x
       y' <- splitThreadContext m t y
       return (ArgCons x' y')

  mergeThreadContext m getSubContext (ArgCons x y) =
    let getSubContextL tc =
          do ArgCons tc' _ <- getSubContext tc
             return tc'
    in
    let getSubContextR tc =
          do ArgCons _ tc' <- getSubContext tc
             return tc'
    in
    do x' <- mergeThreadContext m getSubContextL x
       y' <- mergeThreadContext m getSubContextR y
       return (ArgCons x' y')

instance (ThreadContext r w x, ThreadContext r w y) =>
         ThreadContext r w (x,y) where
  splitThreadContext m t (x,y) =
    do x' <- splitThreadContext m t x
       y' <- splitThreadContext m t y
       return (x', y')

  mergeThreadContext m getSubContext (x,y) =
    let getSubContextL tc =
          do (tc',_) <- getSubContext tc
             return tc'
    in
    let getSubContextR tc =
          do (_,tc') <- getSubContext tc
             return tc'
    in
    do x' <- mergeThreadContext m getSubContextL x
       y' <- mergeThreadContext m getSubContextR y
       return (x',y')

instance ( ThreadContext r w x
         , ThreadContext r w y
         , ThreadContext r w z
         ) =>
         ThreadContext r w (x,y,z) where
  splitThreadContext m t (x,y,z) =
    do x' <- splitThreadContext m t x
       y' <- splitThreadContext m t y
       z' <- splitThreadContext m t z
       return (x', y', z')

  mergeThreadContext m getSubContext (x,y,z) =
    let getSubContext1 tc =
          do (tc',_,_) <- getSubContext tc
             return tc'
    in
    let getSubContext2 tc =
          do (_,tc',_) <- getSubContext tc
             return tc'
    in
    let getSubContext3 tc =
          do (_,_,tc') <- getSubContext tc
             return tc'
    in
    do x' <- mergeThreadContext m getSubContext1 x
       y' <- mergeThreadContext m getSubContext2 y
       z' <- mergeThreadContext m getSubContext3 z
       return (x',y',z')

-- If the initial thread context is Left then splitThreadContext
-- creates only Left thread contexts. Similarly, mergeThreadContext
-- expects all the sub-contexts to match each other.
instance (ThreadContext r w x, ThreadContext r w y) =>
         ThreadContext r w (Either x y) where
  splitThreadContext m t e =
    case e of
      Left x
        -> do x' <- splitThreadContext m t x
              return (Left x')

      Right y
        -> do y' <- splitThreadContext m t y
              return (Right y')

  mergeThreadContext m getSubContext e =
    let getSubContextL tc =
          do Left tc' <- getSubContext tc
             return tc'
    in
    let getSubContextR tc =
          do Right tc' <- getSubContext tc
             return tc'
    in
    case e of
      Left tc
        -> do tc' <- mergeThreadContext m getSubContextL tc
              return (Left tc')

      Right tc
        -> do tc' <- mergeThreadContext m getSubContextR tc
              return (Right tc')

{-|

Every instrument must define an instance of this class for each of its
passes. For example, the
'Control.Monad.MultiPass.Instrument.Counter.Counter' instrument
defines the following instances:

> instance Instrument tc () () () (Counter i r w Off Off tc)
>
> instance Num i =>
>          Instrument tc (CounterTC1 i r) () (Counter i r w On Off tc)
>
> instance Num i =>
>          Instrument tc (CounterTC2 i r) () (Counter i r w On On tc)

The functional dependency from @instr@ to @tc@ and @gc@ enables the
'run' function to automatically deduce the type of the thread context
and global context for each pass.
-}
class Instrument rootTC tc gc instr | instr -> tc gc where
  createInstrument
    :: ST2ToMP rootTC
    -> UpdateThreadContext rootTC tc
    -> gc                          -- ^ Global context
    -> WrapInstrument instr        -- ^ Instrument

-- | This abstract datatype is used as the result type of
-- createInstrument. Instrument authors can create it using the
-- 'wrapInstrument' function, but cannot unwrap it. This ensures that
-- instruments can only be constructed by the "Control.Monad.MultiPass"
-- library.
newtype WrapInstrument instr
  = WrapInstrument instr
    deriving Functor

instance Monad WrapInstrument where
  return x = WrapInstrument x
  WrapInstrument x >>= f = f x

-- | Create an object of type 'WrapInstrument'. It is needed when
-- defining a new instance of the 'Instrument' class.
wrapInstrument :: instr -> WrapInstrument instr
wrapInstrument = WrapInstrument

-- | The type of the first argument of 'createInstrument'. It enables
-- instruments to run 'ST2' in the 'MultiPassBase' monad. (Clearly the
-- @st2ToMP@ argument needs to be used with care.)
type ST2ToMP tc
  = forall r w a. ST2 r w a -> MultiPassBase r w tc a

-- | The type of the first argument of 'createInstrument'. It used to
-- read and write the thread context.
type UpdateThreadContext tc tc'
  = forall r w. (tc' -> tc') -> MultiPassBase r w tc tc'

updateCtxArgL
  :: UpdateThreadContext rootTC (ArgCons tc tcs)
  -> UpdateThreadContext rootTC tc
updateCtxArgL updateCtx h =
  do ArgCons x _ <- updateCtx (mapArgCons h id)
     return x

updateCtxArgR
  :: UpdateThreadContext rootTC (ArgCons tc tcs)
  -> UpdateThreadContext rootTC tcs
updateCtxArgR updateCtx h =
  do ArgCons _ y <- updateCtx (mapArgCons id h)
     return y

class ApplyArg r w param instr f oldTC oldGC tc gc rootTC f'
             | f -> f' tc gc where
  applyArg
    :: PassNumber
    -> StepDirection
    -> param
    -> (instr -> f)
    -> UpdateThreadContext rootTC tc
    -> oldTC
    -> oldGC
    -> ST2 r w (f', tc, gc)

instance ( ApplyArgs r w f oldTCs oldGCs tcs gcs rootTC f'
         , NextThreadContext r w oldTC oldGC tc
         , NextGlobalContext r w oldTC oldGC gc
         , Instrument rootTC tc gc instr
         ) =>
         ApplyArg r w param instr f
                  (ArgCons oldTC oldTCs) (ArgCons oldGC oldGCs)
                  (ArgCons tc tcs) (ArgCons gc gcs)
                  rootTC f' where
  applyArg n d _ f updateCtx
           (ArgCons oldTC oldTCs) (ArgCons oldGC oldGCs) =
    do gc <- nextGlobalContext n d oldTC oldGC
       tc <- nextThreadContext n d oldTC oldGC
       let st2ToMP m = MultiPassBase $ lift m
       let WrapInstrument instr =
             createInstrument st2ToMP (updateCtxArgL updateCtx) gc
       (f', tcs, gcs) <-
         applyArgs n d (f instr) (updateCtxArgR updateCtx) oldTCs oldGCs
       return (f', ArgCons tc tcs, ArgCons gc gcs)

class ApplyArgs r w f oldTC oldGC tc gc rootTC f' | f -> f' tc gc where
  applyArgs
    :: PassNumber
    -> StepDirection
    -> f
    -> UpdateThreadContext rootTC tc
    -> oldTC
    -> oldGC
    -> ST2 r w (f', tc, gc)

instance ApplyArg r w () instr f oldTC oldGC tc gc rootTC f' =>
         ApplyArgs r w (instr -> f) oldTC oldGC tc gc rootTC f' where
  applyArgs n d f updateCtx oldTC oldGC =
    applyArg n d () f updateCtx oldTC oldGC

instance ApplyArg r w param instr f oldTC oldGC tc gc rootTC f' =>
         ApplyArgs r w (Param param (instr -> f)) oldTC oldGC
                   tc gc rootTC f' where
  applyArgs n d (Param param f) updateCtx oldTC oldGC =
    applyArg n d param f updateCtx oldTC oldGC

instance ApplyArgs r w (MultiPassMain r w rootTC a)
                   ArgNil ArgNil ArgNil ArgNil
                   rootTC (MultiPassMain r w rootTC a) where
  applyArgs _ _ f _ ArgNil ArgNil =
    return (f, ArgNil, ArgNil)

class InitCtx ctx where
  initCtx :: ctx

instance InitCtx () where
  initCtx = ()

instance InitCtx ArgNil where
  initCtx = ArgNil

instance (InitCtx a , InitCtx b) =>
         InitCtx (ArgCons a b) where
  initCtx = ArgCons initCtx initCtx

-- | This class is used to create the next thread context when the
-- multi-pass algorithm proceeds to the next pass or back-tracks to
-- the previous pass.
class NextThreadContext r w tc gc tc' where
  nextThreadContext
    :: PassNumber
    -> StepDirection  -- Stepping forwards or backwards?
    -> tc             -- Old thread context
    -> gc             -- Old global context
    -> ST2 r w tc'    -- New thread context

instance NextThreadContext r w tc gc () where
  nextThreadContext _ _ _ _ = return ()

instance ( NextThreadContext r w x gc x'
         , NextThreadContext r w y gc y'
         ) =>
         NextThreadContext r w (x,y) gc (x',y') where
  nextThreadContext n d (x,y) gc =
    do x' <- nextThreadContext n d x gc
       y' <- nextThreadContext n d y gc
       return (x',y')

instance ( NextThreadContext r w () gc x
         , NextThreadContext r w () gc y
         ) =>
         NextThreadContext r w () gc (x,y) where
  nextThreadContext n d () gc =
    do x <- nextThreadContext n d () gc
       y <- nextThreadContext n d () gc
       return (x,y)

instance ( NextThreadContext r w x gc x'
         , NextThreadContext r w y gc y'
         , NextThreadContext r w z gc z'
         ) =>
         NextThreadContext r w (x,y,z) gc (x',y',z') where
  nextThreadContext n d (x,y,z) gc =
    do x' <- nextThreadContext n d x gc
       y' <- nextThreadContext n d y gc
       z' <- nextThreadContext n d z gc
       return (x',y',z')

instance ( NextThreadContext r w () gc x
         , NextThreadContext r w () gc y
         , NextThreadContext r w () gc z
         ) =>
         NextThreadContext r w () gc (x,y,z) where
  nextThreadContext n d () gc =
    do x <- nextThreadContext n d () gc
       y <- nextThreadContext n d () gc
       z <- nextThreadContext n d () gc
       return (x,y,z)

instance ( NextThreadContext r w x gc x'
         , NextThreadContext r w y gc y'
         ) =>
         NextThreadContext r w (Either x y) gc (Either x' y') where
  nextThreadContext n d e gc =
    case e of
      Left x
        -> do x' <- nextThreadContext n d x gc
              return (Left x')

      Right y
        -> do y' <- nextThreadContext n d y gc
              return (Right y')


-- | This class is used to create the next global context when the
-- multi-pass algorithm proceeds to the next pass or back-tracks to
-- the previous pass.
class NextGlobalContext r w tc gc gc' where
  nextGlobalContext
    :: PassNumber
    -> StepDirection  -- Stepping forwards or backwards?
    -> tc             -- Old thread context
    -> gc             -- Old global context
    -> ST2 r w gc'    -- New global context

instance NextGlobalContext r w tc gc () where
  nextGlobalContext _ _ _ _ = return ()

instance ( NextGlobalContext r w tc x x'
         , NextGlobalContext r w tc y y'
         ) =>
         NextGlobalContext r w tc (x,y) (x',y') where
  nextGlobalContext n d tc (x,y) =
    do x' <- nextGlobalContext n d tc x
       y' <- nextGlobalContext n d tc y
       return (x',y')

instance ( NextGlobalContext r w tc x x'
         , NextGlobalContext r w tc y y'
         , NextGlobalContext r w tc z z'
         ) =>
         NextGlobalContext r w tc (x,y,z) (x',y',z') where
  nextGlobalContext n d tc (x,y,z) =
    do x' <- nextGlobalContext n d tc x
       y' <- nextGlobalContext n d tc y
       z' <- nextGlobalContext n d tc z
       return (x',y',z')

instance ( NextGlobalContext r w tc x x'
         , NextGlobalContext r w tc y y'
         ) =>
         NextGlobalContext r w tc (Either x y) (Either x' y') where
  nextGlobalContext n d tc e =
    case e of
      Left x
        -> do x' <- nextGlobalContext n d tc x
              return (Left x')

      Right y
        -> do y' <- nextGlobalContext n d tc y
              return (Right y')

class InstantiatePasses a b | a -> b where
  instantiatePasses :: a -> PassZ b

instance InstantiatePasses (PassZ a) a where
  instantiatePasses (PassZ x) = PassZ x

instance InstantiatePasses (cont (m Off)) b =>
         InstantiatePasses (PassS cont m) b where
  instantiatePasses (PassS f) =
    instantiatePasses (f :: cont (m Off))

-- | Every instrument must define an instance of this class for each
-- of its passes. It is used to tell the evaluator whether it needs to
-- back-track. Instruments which do not back-track should use the
-- default implementation of backtrack which returns 'Nothing' (which
-- means that no back-tracking is necessary.) If more than one
-- instrument requests that the evaluator back-tracks then the
-- evaluator will back-track to the earliest of the requested passes.
class BackTrack r w tc gc where
  backtrack :: tc -> gc -> ST2 r w (Maybe PassNumber)
  backtrack _ _ = return Nothing

-- If the global context is the unit type then the instrument does not
-- back-track.
instance BackTrack r w tc ()

instance BackTrack r w ArgNil ArgNil

instance (BackTrack r w tc gc, BackTrack r w tcs gcs) =>
         BackTrack r w (ArgCons tc tcs) (ArgCons gc gcs) where
  backtrack (ArgCons tc tcs) (ArgCons gc gcs) =
    do mx <- backtrack tc gc
       my <- backtrack tcs gcs
       case (mx,my) of
         (Nothing, Nothing) -> return Nothing
         (Nothing, Just y)  -> return (Just y)
         (Just x, Nothing)  -> return (Just x)
         (Just x, Just y)   -> return (Just (minPassNumber x y))

class RunPasses r w f tc gc p out where
  runPasses
    :: PassNumber -> f -> p out -> tc -> gc
    -> ST2 r w
        (Either
           ( PassNumber
           , MultiPassMain r w tc (p out)
           , tc
           , gc
           )
           out)

instance RunPasses r w (PassZ f) tc gc On out where
  runPasses _ _ (On out) _ _ =
    return (Right out)

instance ( InstantiatePasses (cont (f Off)) fPrev
         , MultiPassAlgorithm (fPrev tc0) gPrev
         , InstantiatePasses (cont (f On)) fCurr
         , MultiPassAlgorithm (fCurr tc1) gCurr
         , ApplyArgs r w gCurr tc0 gc0 tc1 gc1 tc1
                     (MultiPassMain r w tc1 (p out))
         , ApplyArgs r w gCurr tc1 gc1 tc1 gc1 tc1
                     (MultiPassMain r w tc1 (p out))
         , ApplyArgs r w gPrev tc1 gc1 tc0 gc0 tc0
                     (MultiPassMain r w tc0 (q out))
         , ThreadContext r w tc1
         , BackTrack r w tc1 gc1
         , RunPasses r w (cont (f On)) tc1 gc1 p out
         ) =>
         RunPasses r w (PassS cont f) tc0 gc0 q out where
  runPasses n fBox _ =
    let PassS (fPrev :: cont (f Off)) = fBox in
    let PassS (fCurr :: cont (f On)) = fBox in
    let -- Loop header. Run the current pass and check whether
        -- back-tracking is necessary.
        loop g tc gc =
          do (result, tc') <- runMultiPassMain g tc
             mb <- backtrack tc' gc
             case mb of
               Nothing
                 -> -- Current pass is successful, so continue to
                    -- the next pass.
                    let n' = incrPassNumber n in
                    do e <- runPasses n' fCurr result tc' gc
                       case e of
                         Left info -> rewind info
                         Right out -> return (Right out)

               Just m
                 -> stepReset m tc' gc

        -- Call either loop or stepBackward, depending on the
        -- PassNumber.
        rewind (m,g,tc,gc) =
          assert (unwrapPassNumber m <= unwrapPassNumber n) $
          if unwrapPassNumber m == unwrapPassNumber n
             then loop g tc gc
             else stepBackward m tc gc

        -- Reset the contexts and rewind to the requested pass number.
        stepReset m tc gc =
          let PassZ f' = instantiatePasses fCurr in
          let g = unwrapMultiPassAlgorithm (f' :: fCurr tc1) in
          do (g', tc', gc') <-
               applyArgs n StepReset g updateThreadContextTop tc gc
             rewind (m,g',tc',gc')

        -- Return to the previous pass.
        stepBackward m tc gc =
          let PassZ f' = instantiatePasses fPrev in
          let g = unwrapMultiPassAlgorithm (f' :: fPrev tc0) in
          do (g', tc', gc') <-
               applyArgs n StepBackward g updateThreadContextTop tc gc
             return (Left (m,g',tc',gc'))
    in
    let loopStart tc gc =
          let PassZ f' = instantiatePasses fCurr in
          let g = unwrapMultiPassAlgorithm (f' :: fCurr tc1) in
          do (g', tc', gc') <-
               applyArgs n StepForward g updateThreadContextTop tc gc
             loop g' tc' gc'
    in
    loopStart

updateThreadContextTop :: UpdateThreadContext tc tc
updateThreadContextTop f =
  MultiPassBase $
  do tc <- get
     put (f tc)
     return tc

-- | This function is used to run a multi-pass algorithm. Its
-- complicated type is mostly an artifact of the internal
-- implementation, which uses type classes to generate the code for
-- each pass of the algorithm. Therefore, the recommended way to learn
-- how to use 'run' is to look at some of the examples in the
-- @Example@ sub-directory.
run
  :: forall r w f f' g tc gc out.
     ( InstantiatePasses f f'
     , MultiPassAlgorithm (f' tc) g
     , ApplyArgs r w g tc gc tc gc tc
                 (MultiPassMain r w tc (Off out))
     , InitCtx tc
     , InitCtx gc
     , RunPasses r w f tc gc Off out
     )
  => f
  -> ST2 r w out
run f =
  let tc = initCtx :: tc in
  let gc = initCtx :: gc in
  do e <- runPasses (PassNumber 0) f Off tc gc
     case e of
       Left _
         -> -- This is impossible, because it would imply that the
            -- back-tracking mechanism is attempting to back-track to
            -- a negative PassNumber.
            assert False $ error "run"

       Right result
         -> return result

-- | 'NumThreads' is used to specify the number of threads in
-- 'parallelMP' and 'parallelMP_'.
newtype NumThreads
  = NumThreads Int

-- | Use @m@ threads to run @n@ instances of the function @f@. The
-- results are returned in an array of length @n@.
parallelMP
  :: (Ix i, Num i)
  => NumThreads                 -- ^ Number of threads to spawn
  -> (i,i)                      -- ^ Element range
  -> (i -> MultiPass r w tc a)
  -> MultiPass r w tc (ST2Array r w i a)
parallelMP (NumThreads m) bnds f =
  let n = rangeSize bnds in
  assert (m > 0) $
  if m == 1 || n <= 1
     then -- Do not use parallelism.
          do xs <- MultiPass $ MultiPassBase $ lift $ newST2Array_ bnds
             sequence_
               [ do x <- f i
                    MultiPass $ MultiPassBase $ lift $
                      writeST2Array xs i x
               | i <- range bnds
               ]
             return xs
     else assert (m > 1) $
          assert (n > 1) $
          parallelHelper (min m n) n bnds f

parallelHelper
  :: (Ix i, Num i)
  => Int                        -- Number of threads
  -> Int                        -- Number of elements
  -> (i,i)                      -- Element range
  -> (i -> MultiPass r w tc a)
  -> MultiPass r w tc (ST2Array r w i a)
parallelHelper m n bnds f =
  MultiPass $ MultiPassBase $
  do tc <- get
     -- Split the thread state into m sub-states.
     let tBnds = (0,m-1)
     tcs <- lift $ newST2Array_ tBnds
     lift $ sequence_
       [ do tci <- splitThreadContext m t tc
            writeST2Array tcs t tci
       | t <- range tBnds
       ]
     -- Create an array for the results.
     xs <- lift $ newST2Array_ bnds
     let base = fst bnds
     let blockSize = (n+m-1) `div` m
     lift $ parallelST2 tBnds $ \i ->
       do tci <- readST2Array tcs i
          let start = i * blockSize
          let end = min n (start + blockSize)
          tci' <-
            flip execStateT tci $
            sequence_
              [ let j' = base + fromIntegral j in
                do x <- unwrapMultiPassBase $ unwrapMultiPass $ f j'
                   lift $ writeST2Array xs j' x
              | j <- [start .. end-1]
              ]
          writeST2Array tcs i tci'
     -- Create the new merged state.
     tc' <- lift $ mergeThreadContext m (readST2Array tcs) tc
     put tc'
     return xs

-- | Modified version of 'parallelMP' which discards the result of the
-- function, rather than writing it to an array.
parallelMP_
  :: (Ix i, Num i)
  => NumThreads                 -- ^ Number of threads to spawn
  -> (i,i)                      -- ^ Element range
  -> (i -> MultiPass r w tc a)
  -> MultiPass r w tc ()
parallelMP_ (NumThreads m) bnds f =
  let n = rangeSize bnds in
  assert (m > 0) $
  if m == 1 || n <= 1
     then -- Do not use parallelism.
          sequence_ [ f i | i <- range bnds ]
     else assert (m > 1) $
          assert (n > 1) $
          parallelHelper_ (min m n) n bnds f

parallelHelper_
  :: (Ix i, Num i)
  => Int                        -- Number of threads
  -> Int                        -- Number of elements
  -> (i,i)                      -- Element range
  -> (i -> MultiPass r w tc a)
  -> MultiPass r w tc ()
parallelHelper_ m n bnds f =
  MultiPass $ MultiPassBase $
  do tc <- get
     -- Split the thread state into m sub-states.
     let tBnds = (0,m-1)
     tcs <- lift $ newST2Array_ tBnds
     lift $ sequence_
       [ do tci <- splitThreadContext m t tc
            writeST2Array tcs t tci
       | t <- range tBnds
       ]
     let base = fst bnds
     let blockSize = (n+m-1) `div` m
     lift $ parallelST2 tBnds $ \i ->
       do tci <- readST2Array tcs i
          let start = i * blockSize
          let end = min n (start + blockSize)
          tci' <-
            flip execStateT tci $
            sequence_
              [ let j' = base + fromIntegral j in
                unwrapMultiPassBase $ unwrapMultiPass $ f j'
              | j <- [start .. end-1]
              ]
          writeST2Array tcs i tci'
     -- Create the new merged state.
     tc' <- lift $ mergeThreadContext m (readST2Array tcs) tc
     put tc'

-- | Read-only ST2 computations are allowed to be executed in the
-- MultiPass monad.
readOnlyST2ToMP :: (forall w. ST2 r w a) -> MultiPass r w' tc a
readOnlyST2ToMP m =
  MultiPass $ MultiPassBase $
  lift m
