{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples  #-}
{-# LANGUAGE UnliftedFFITypes #-}

module GHC.VacuumTube.Limbo
  ( Limbo
  , LimboMap
  , vacuumGet
  ) where

import Control.Monad.ST.Unsafe (unsafeIOToST)
import Data.Array.Base
import Data.Binary
import Data.Foldable
import Data.Graph
import Data.Map.Lazy as Map
import Data.Monoid hiding (Any)
import Data.STRef
import Data.Set as Set

import GHC.Prim
import GHC.Ptr
import GHC.ST
import GHC.Types

import GHC.VacuumTube.EncodedState
import GHC.VacuumTube.Types
import GHC.VacuumTube.VacuumNode

import Debug.Trace

foreign import prim "VacuumAssembler_allocateClosure" unsafeAllocateClosure :: Addr# -> Word# -> Word# -> State# s -> (# State# s, Any #)
foreign import prim "VacuumAssembler_allocateThunk" unsafeAllocateThunk :: Addr# -> Word# -> Word# -> State# s -> (# State# s, Any #)
foreign import prim "VacuumAssembler_setPtr" unsafeSetPtr :: Any -> Word# -> Any -> State# s -> (# State# s #)
foreign import prim "VacuumAssembler_setNPtr" unsafeSetNPtr :: Any -> Word# -> Word# -> State# s -> (# State# s #)
foreign import prim "VacuumAssembler_indirectByteArray" unsafeIndirectByteArray :: Any -> ByteArray# -> State# s -> (# State# s #)
foreign import prim "VacuumAssembler_getInfoTable" unsafeGetInfoTable :: Any -> State# s -> (# State# s, Addr# #)

type LimboMap s a = ClosureMap (Limbo s a)

countPayload :: Foldable t => t Payload -> (Ptrs, NPtrs)
countPayload = e . foldMap s where
  e (Sum x, Sum y) = (x, y)
  s PtrPayload{}   = (Sum 1, Sum 0)
  s NPtrPayload{}  = (Sum 0, Sum 1)

allocateClosures :: LimboMap s Any -> SCC (Ptr Closure, VacuumNode) -> ST s (LimboMap s Any)
allocateClosures limboMap (AcyclicSCC (closure, node)) = case node of
  VacuumClosure infoTable payload -> do
    let (ptrs, nptrs) = countPayload payload
    l <- limboClosure infoTable ptrs nptrs
    setPayload limboMap l payload
    return $! Map.insert closure l limboMap

  VacuumThunk infoTable payload -> do
    let (ptrs, nptrs) = countPayload payload
    l <- limboThunk infoTable ptrs nptrs
    setPayload limboMap l payload
    return $! Map.insert closure l limboMap

  VacuumArray infoTable payload -> do
    l <- limboArray infoTable payload
    return $! Map.insert closure l limboMap

  VacuumStatic -> do
    l <- limboStatic closure
    return $! Map.insert closure l limboMap

setPayload :: LimboMap s Any -> Limbo s a -> Map Word Payload -> ST s ()
setPayload limboMap l = traverse_ (uncurry step) . Map.toList where
  step k (PtrPayload p)
    | Just p' <- Map.lookup p limboMap = trace ("ptr match") $ setPtr l k p'
    | otherwise = trace ("ptr nomatch") $ error "foo" -- setPtr l k (vacuumInfoTable 
  step k (NPtrPayload p) = trace "nptr" $ setNPtr l k p

sccNodeMap :: Map (Ptr Closure) VacuumNode -> [SCC (Ptr Closure, VacuumNode)]
sccNodeMap = fmap swapAndDropSCC . scc . Map.toList where
  scc = stronglyConnCompR . fmap pieces
  ptrElems p = [p | PtrPayload p <- Map.elems p]
  pieces (k, v) = (v, k, ptrs v)
  swapAndDrop (v, k, _) = (k, v)
  swapAndDropSCC (AcyclicSCC x) = AcyclicSCC (swapAndDrop x)
  swapAndDropSCC (CyclicSCC xs) = CyclicSCC (fmap swapAndDrop xs)
  ptrs VacuumClosure{vacuumPayload} = ptrElems vacuumPayload
  ptrs VacuumThunk  {vacuumPayload} = ptrElems vacuumPayload
  ptrs VacuumArray  {} = []
  ptrs VacuumStatic {} = []

vacuumGet :: Get a
vacuumGet = do
  EncodedState (Just (root, _)) [] nodeMap <- get
  runST $ do
    limboMap <- foldlM allocateClosures Map.empty (sccNodeMap nodeMap)

    case Map.lookup root limboMap of
      Just limboRoot -> do
        mval <- ascend limboRoot
        return $ case mval of
          Just val -> return val
          Nothing  -> fail "Ascension failed"
      Nothing -> return $ fail "Root closure not found after construction"

data Limbo s a = Limbo {limboPtrs :: STRef s (Set Word), limboNPtrs :: STRef s (Set Word), limboVal :: a}

allocateClosure :: Ptr InfoTable -> Ptrs -> NPtrs -> ST s Any
allocateClosure (Ptr infoTable#) (W# ptrs#) (W# nptrs#) = ST  $ \ st ->
  case unsafeAllocateClosure infoTable# ptrs# nptrs# st of
    (# st', clos# #) -> (# st', clos# #)

allocateThunk :: Ptr InfoTable -> Ptrs -> NPtrs -> ST s Any
allocateThunk (Ptr infoTable#) (W# ptrs#) (W# nptrs#) = ST  $ \ st ->
  case unsafeAllocateThunk infoTable# ptrs# nptrs# st of
    (# st', clos# #) -> (# st', clos# #)

limboClosure :: forall a s . Ptr InfoTable -> Ptrs -> NPtrs -> ST s (Limbo s a)
limboClosure infoTable ptrs nptrs = do
  let ptrSet  = Set.fromList [i-1      | i <- [1..ptrs]]
      nptrSet = Set.fromList [i+ptrs-1 | i <- [1..nptrs]]

  clos <- traceShow ("allocating", infoTable, ptrs, nptrs) $ allocateClosure infoTable ptrs nptrs

  ptrRef  <- newSTRef ptrSet
  nptrRef <- newSTRef nptrSet

  return Limbo{limboPtrs = ptrRef, limboNPtrs = nptrRef, limboVal = unsafeCoerce# clos :: a}

limboThunk :: forall a s . Ptr InfoTable -> Ptrs -> NPtrs -> ST s (Limbo s a)
limboThunk infoTable ptrs nptrs = do
  let ptrSet  = Set.fromList [i-1      | i <- [1..ptrs]]
      nptrSet = Set.fromList [i+ptrs-1 | i <- [1..nptrs]]

  clos <- allocateThunk infoTable ptrs nptrs

  ptrRef  <- newSTRef ptrSet
  nptrRef <- newSTRef nptrSet

  return Limbo{limboPtrs = ptrRef, limboNPtrs = nptrRef, limboVal = unsafeCoerce# clos :: a}

limboArray :: forall a s . Ptr InfoTable -> UArray Word Word8 -> ST s (Limbo s a)
limboArray _infoTable (UArray _ _ _ arr) = do
  ptrRef  <- newSTRef Set.empty
  nptrRef <- newSTRef Set.empty

  return Limbo{limboPtrs = ptrRef, limboNPtrs = nptrRef, limboVal = unsafeCoerce# arr :: a}

limboStatic :: forall a s . Ptr Closure -> ST s (Limbo s a)
limboStatic (Ptr clos#) = do
  ptrRef  <- newSTRef Set.empty
  nptrRef <- newSTRef Set.empty

  return Limbo{limboPtrs = ptrRef, limboNPtrs = nptrRef, limboVal = unsafeCoerce# clos# :: a}

setByteArray :: Limbo s a -> Word -> ByteArray# -> ST s ()
setByteArray Limbo{limboPtrs = ptrRef, limboVal = closure} slot arr# = do
  ST $ \ st -> case unsafeIndirectByteArray (unsafeCoerce# closure :: Any) arr# st of
    (# st' #) -> (# st', () #)

  modifySTRef' ptrRef (Set.delete slot)

setPtr :: Limbo s a -> Word -> Limbo s Any -> ST s ()
{-# NOINLINE setPtr #-}
setPtr Limbo{limboPtrs = ptrRef, limboVal = closure} slot@(W# slot#) Limbo{limboVal = val} = do
  ST $ \ st -> case unsafeSetPtr (unsafeCoerce# closure :: Any) slot# val st of
    (# st' #) -> (# st', () #)

  modifySTRef' ptrRef (Set.delete slot)

setNPtr :: Limbo s a -> Word -> Word -> ST s ()
setNPtr Limbo{limboNPtrs = nptrRef, limboVal = closure} slot@(W# slot#) (W# val#) = do
  ST $ \ st -> case unsafeSetNPtr (unsafeCoerce# closure :: Any) slot# val# st of
    (# st' #) -> (# st', () #)

  modifySTRef' nptrRef (Set.delete slot)

ascend :: forall a s . Limbo s Any -> ST s (Maybe a)
{-# NOINLINE ascend #-}
ascend Limbo{limboPtrs = ptrRef, limboNPtrs = nptrRef, limboVal = val} = do
  ptrSet <- readSTRef ptrRef
  nptrSet <- readSTRef nptrRef
  return $ case (Set.null ptrSet, Set.null nptrSet) of
    (True, True) -> Just (unsafeCoerce# val :: a)
    _            -> Nothing
