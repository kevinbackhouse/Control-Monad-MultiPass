-- Copyright 2013 Kevin Backhouse.

{-|
The 'OrdCons' instrument uses two passes to implement hash-consing.
The values are added to the table during the first pass and a unique
index for each value is returned during the second pass.

'OrdCons' is implemented using 'Data.Map', so it can be used on any
datatype which is an instance of 'Ord'.
-}

module Control.Monad.MultiPass.Instrument.OrdCons
  ( OrdCons
  , initOrdCons, ordCons, getOrdConsTable
  , OrdConsTable
  , lookupOrdConsTable, insertOrdConsTable, growOrdConsTable
  )
where

import Control.Exception ( assert )
import Control.Monad.ST2
import Control.Monad.Writer.Strict
import Control.Monad.MultiPass
import Control.Monad.MultiPass.ThreadContext.MonoidTC
import qualified Data.Map as FM
import Data.Maybe ( isJust, fromJust )

-- | Abstract datatype for the instrument.
data OrdCons a r w p1 p2 tc
  = OrdCons
      { initInternal
          :: !(p1 (OrdConsTable a) -> MultiPassPrologue r w tc ())

      , ordConsInternal
          :: !(p1 a -> MultiPass r w tc (p2 Int))

      , getOrdConsTableInternal
          :: !(MultiPassEpilogue r w tc (p2 (OrdConsTable a)))
      }

-- | Initialise the 'OrdCons' instrument with an 'OrdConsTable'.  This
-- method is optional. Ff this method is not used then the instrument
-- will be initialised with an empty 'OrdConsTable'.
initOrdCons
  :: (Ord a, Monad p1, Monad p2)
  => OrdCons a r w p1 p2 tc       -- ^ Instrument
  -> p1 (OrdConsTable a)          -- ^ Initial table
  -> MultiPassPrologue r w tc ()
initOrdCons =
  initInternal

-- | Get a unique index for the value.
ordCons
  :: (Ord a, Monad p1, Monad p2)
  => OrdCons a r w p1 p2 tc       -- ^ Instrument
  -> p1 a                         -- ^ Value
  -> MultiPass r w tc (p2 Int)    -- ^ Unique index
ordCons =
  ordConsInternal

-- | Get the final 'OrdConsTable'.
getOrdConsTable
  :: OrdCons a r w p1 p2 tc
  -> MultiPassEpilogue r w tc (p2 (OrdConsTable a))
getOrdConsTable =
  getOrdConsTableInternal

-- | This datatype is a newtype around @'FM.Map' a 'Int'@. It maps its
-- keys (of type @a@) to a permutation of the integers @0..n-1@, where
-- @n@ is the number of keys.
newtype OrdConsTable a
  = OrdConsTable (FM.Map a Int)

-- | Empty 'OrdConsTable'.
emptyOrdConsTable :: OrdConsTable a
emptyOrdConsTable =
  OrdConsTable FM.empty

-- | Lookup an element.
lookupOrdConsTable :: Ord a => OrdConsTable a -> a -> Maybe Int
lookupOrdConsTable (OrdConsTable table) x =
  FM.lookup x table

-- | Insert an element. If the element is not in the map yet, then it
-- is assigned index @n@, where @n@ is the original size of the table.
insertOrdConsTable :: Ord a => OrdConsTable a -> a -> OrdConsTable a
insertOrdConsTable (OrdConsTable table) x =
  if FM.member x table
     then OrdConsTable table
     else OrdConsTable $ FM.insert x (FM.size table) table

-- | Add multiple elements. The new elements are assigned indices
-- @n..n+k-1@, where @n@ is the original size of the table and @k@ is
-- the number of new elements to be added. This function will assert
-- if any of the new elements are already in the table.
growOrdConsTable
  :: Ord a => OrdConsTable a -> FM.Map a () -> OrdConsTable a
growOrdConsTable (OrdConsTable table) xs =
  assert (FM.null (FM.intersection table xs)) $
  let n = FM.size table in
  let xs' = snd $ FM.mapAccum (\i () -> (i+1, i)) n xs in
  OrdConsTable $ FM.union table xs'

newtype GC1 r w a
  = GC1 (ST2Ref r w (OrdConsTable a))

newtype OrdConsTC a
  = OrdConsTC (FM.Map a ())

instance Ord a => Monoid (OrdConsTC a) where
  mempty =
    OrdConsTC FM.empty

  mappend (OrdConsTC xs) (OrdConsTC ys) =
    OrdConsTC (FM.union xs ys)

instance Instrument tc () ()
                    (OrdCons a r w Off Off tc) where
  createInstrument _ _ () =
    wrapInstrument $ OrdCons
      { initInternal = \Off -> return ()
      , ordConsInternal = \Off -> return Off
      , getOrdConsTableInternal = return Off
      }

instance Ord a =>
         Instrument tc (MonoidTC (OrdConsTC a)) (GC1 r w a)
                    (OrdCons a r w On Off tc) where
  createInstrument st2ToMP updateCtx (GC1 initTableRef) =
    wrapInstrument $ OrdCons
      { initInternal = \(On initTable) ->
          mkMultiPassPrologue $
          do -- Check that the initTableRef has not been initialised
             -- already.
             OrdConsTable xs <- st2ToMP $ readST2Ref initTableRef
             assert (FM.null xs) $ return ()
             st2ToMP $ writeST2Ref initTableRef initTable

      , ordConsInternal = \(On x) ->
          let updateTable initTable (MonoidTC (OrdConsTC table)) =
                MonoidTC $ OrdConsTC $
                if isJust (lookupOrdConsTable initTable x)
                   then table
                   else FM.insert x () table
          in
          mkMultiPass $
          do initTable <- st2ToMP $ readST2Ref initTableRef
             _ <- updateCtx (updateTable initTable)
             return Off

      , getOrdConsTableInternal =
          return Off
      }

-- The gc2_newTable field is a superset of gc2_initTable. (The
-- initTable is only used if back-tracking occurs.)
data GC2 a
  = GC2
      { gc2_initTable :: !(OrdConsTable a)
      , gc2_newTable  :: !(OrdConsTable a)
      }

instance Ord a => Instrument tc () (GC2 a)
                             (OrdCons a r w On On tc) where
  createInstrument _ _ gc =
    let newTable = gc2_newTable gc in
    wrapInstrument $ OrdCons
      { initInternal = \(On _) -> return ()

      , ordConsInternal = \(On x) ->
          let m = lookupOrdConsTable newTable x in
          assert (isJust m) $
          return $ On $ fromJust m

      , getOrdConsTableInternal =
          return (On newTable)
      }

-- This instrument never needs to back-track.
instance BackTrack r w tc (GC1 r w a)
instance BackTrack r w () (GC2 a)

instance NextGlobalContext r w () () (GC1 r w a) where
  nextGlobalContext _ _ () () =
    do initTableRef <- newST2Ref emptyOrdConsTable
       return (GC1 initTableRef)

instance NextGlobalContext r w tc (GC1 r w a) (GC1 r w a) where
  nextGlobalContext _ _ _ gc =
    return gc

instance Ord a =>
         NextGlobalContext r w (MonoidTC (OrdConsTC a))
                           (GC1 r w a) (GC2 a) where
  nextGlobalContext _ _ tc gc =
    let GC1 initTableRef = gc in
    let MonoidTC (OrdConsTC table) = tc in
    do initTable <- readST2Ref initTableRef
       return $ GC2
         { gc2_initTable = initTable
         , gc2_newTable  = growOrdConsTable initTable table
         }

instance NextGlobalContext r w tc (GC2 a) (GC2 a) where
  nextGlobalContext _ _ _ gc =
    return gc

instance NextGlobalContext r w tc (GC2 a) (GC1 r w a) where
  nextGlobalContext _ _ _ gc =
    do initTableRef <- newST2Ref (gc2_initTable gc)
       return (GC1 initTableRef)
