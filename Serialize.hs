{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples  #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Serialize where

import Control.Applicative
import Control.Monad.ST.Unsafe (unsafeIOToST)
import Data.Array.Base
import Data.Binary
import Data.Foldable
import Data.List as List
import Data.Map.Lazy as Map
import Data.Maybe (fromMaybe)
import Data.Monoid (mappend)
import Data.STRef
import Data.Set as Set

import GHC.Generics
import GHC.Prim
import GHC.Ptr
import GHC.ST
import GHC.Types
import GHC.Word

import Debug.Trace

newtype VacuumPtr a = VacuumPtr{unVacuumPtr :: a}

instance Binary (VacuumPtr (Ptr a)) where
  put (VacuumPtr (Ptr val#)) = put (W# (unsafeCoerce# val# :: Word#))
  get = do
    W# val# <- get
    return . VacuumPtr $ Ptr (unsafeCoerce# val# :: Addr#)

data Closure

instance Binary (Ptr Closure) where
  put = put . VacuumPtr
  get = unVacuumPtr <$> get

data InfoTable

instance Binary (Ptr InfoTable) where
  put = put . VacuumPtr
  get = unVacuumPtr <$> get

data Payload
  = PtrPayload (Ptr Closure)
  | NPtrPayload Word
  | ArrayPayload (UArray Word Word8)
    deriving (Eq, Ord, Generic, Show)

instance Binary Payload

data VacuumNode
  = VacuumClosure {vacuumInfoTable :: Ptr InfoTable, vacuumPayload    :: Map Word Payload}
  | VacuumThunk   {vacuumInfoTable :: Ptr InfoTable, vacuumPayload    :: Map Word Payload}
  | VacuumSelector{vacuumInfoTable :: Ptr InfoTable, vacuumSelectee   :: Ptr Closure}
  | VacuumPAP     {vacuumInfoTable :: Ptr InfoTable, vacuumPayload    :: Map Word Payload, vacuumArity :: Word, vacuumNArgs :: Word, vacuumFun :: Ptr Closure}
  | VacuumAP      {vacuumInfoTable :: Ptr InfoTable, vacuumPayload    :: Map Word Payload, vacuumArity :: Word, vacuumNArgs :: Word, vacuumFun :: Ptr Closure}
  | VacuumAP_STACK{vacuumInfoTable :: Ptr InfoTable, vacuumPayload    :: Map Word Payload, vacuumFun :: Ptr Closure}
  | VacuumInd     {vacuumInfoTable :: Ptr InfoTable, vacuumIndirectee :: Ptr Closure}
  | VacuumStatic
  | VacuumUndefined
    deriving (Eq, Ord, Generic, Show)

instance Binary VacuumNode

type ClosureMap a = Map (Ptr Closure) a
type VacuumMap = ClosureMap VacuumNode
type LimboMap s a = ClosureMap (Limbo s a)

newtype VacuumTube a = VacuumTube{unVacuumTube :: a}

allocateClosures :: LimboMap s Any -> (Ptr Closure, VacuumNode) -> ST s (LimboMap s Any)
allocateClosures = undefined
{-
buildRemap remap VacuumNode{vacuumInfoTable = infoTable, vacuumPtrs = ptrs, vacuumNPtrs = nptrs, vacuumClosure = closure} = do
  unsafeIOToST . traceIO $ show ("limbo", infoTable, closure)
  l <- limbo infoTable ptrs nptrs
  return $! Map.insert closure l remap

buildRemap remap VacuumStatic{vacuumClosure = closure} = do
  unsafeIOToST . traceIO $ show ("lstatic", closure)
  l <- limboStatic closure
  return $! Map.insert closure l remap
-}

keysDifference :: Ord a => Map a b -> Map a c -> Set a
keysDifference x y = Map.keysSet x `Set.difference` Map.keysSet y

setPayload :: LimboMap s Any -> Limbo s a -> Map Word Payload -> ST s ()
setPayload remap l = traverse_ (uncurry step) . Map.toList where
  step k (PtrPayload p)
    | Just p' <- Map.lookup p remap = trace ("ptr match") $ setPtr l k p'
    | otherwise = trace ("ptr nomatch") $ error "foo" -- setPtr l k (vacuumInfoTable 
  step k (NPtrPayload p) = setNPtr l k p
  step k (ArrayPayload (UArray _ _ _ p)) = setByteArray l k p

instance Binary (VacuumTube a) where
  put (VacuumTube val) =
    let st = EncodedState Nothing [] Map.empty
    in put $ runST $ encodeObject val st

  get = do
    EncodedState (Just root) [] nodeMap <- get
    traceShow ("root", root, nodeMap) $ runST $ do
      remap <- foldlM allocateClosures Map.empty (Map.toList nodeMap)
      -- unsafeIOToST . traceIO $ "Top: " ++ show (Map.size (Map.intersectionWith (,) remap nodeMap))
      for_ (Map.intersectionWith (,) remap nodeMap) $ \ (l, v) -> do
        -- unsafeIOToST . traceIO $ "Checking: " ++ show v
        case v of
          VacuumClosure{vacuumPayload} -> setPayload remap l vacuumPayload
          VacuumThunk  {vacuumPayload} -> setPayload remap l vacuumPayload

      case Map.lookup (fst root) remap of
        Just limboRoot -> do
          mval <- ascend limboRoot
          return $ case mval of
            Just val -> return (VacuumTube val :: VacuumTube a)
            Nothing  -> fail "Ascension failed"
        Nothing -> return $ fail "Root closure not found after construction"

data EncodedState = EncodedState
  { encodedRoot  :: Maybe (Ptr Closure, VacuumNode)
  , encodedStack :: [(Ptr Closure, VacuumNode)]
  , encodedNodes :: Map (Ptr Closure) VacuumNode
  } deriving (Show, Generic)

instance Binary EncodedState

foreign import prim "WalkGraph_encodeObject" unsafeEncodeObject :: Any -> Any -> State# s -> (# State# s, Any #)
foreign import prim "WalkGraph_allocateClosure" unsafeAllocateClosure :: Addr# -> Word# -> Word# -> State# s -> (# State# s, Addr# #)
foreign import prim "WalkGraph_setPtr" unsafeSetPtr :: Any -> Word# -> Any -> State# s -> (# State# s #)
foreign import prim "WalkGraph_indirectByteArray" unsafeIndirectByteArray :: Any -> ByteArray# -> State# s -> (# State# s #)
foreign import prim "WalkGraph_setNPtr" unsafeSetNPtr :: Any -> Word# -> Word# -> State# s -> (# State# s #)
foreign import prim "WalkGraph_getInfoTable" unsafeGetInfoTable :: Any -> State# s -> (# State# s, Addr# #)

encodeObject :: a -> EncodedState -> ST s EncodedState
encodeObject val enc = ST $ \ st ->
  case unsafeEncodeObject (unsafeCoerce# val :: Any) (unsafeCoerce# enc :: Any) st of
    (# st', res #) -> (# st', unsafeCoerce# res :: EncodedState #)

popTag :: EncodedState -> EncodedState
popTag (EncodedState _ (x@(ptr, closure):xs) graph) =
  traceShow ("pop", closure) (EncodedState (Just x) xs (Map.insert ptr closure graph))

pushTag :: Word# -> Addr# -> Word# -> Word# -> Addr# -> EncodedState -> EncodedState
pushTag tag# infoTable# ptrs# nptrs# closure# st@EncodedState{encodedStack = xs, encodedNodes} =
  let ptr = Ptr closure#
      node = VacuumClosure{vacuumInfoTable = Ptr infoTable#, vacuumPayload = Map.empty}
      encst = st{encodedStack = (Ptr closure#, VacuumStatic):xs, encodedNodes = Map.insert ptr VacuumUndefined encodedNodes}
  in traceShow ("push", W# tag#, Ptr infoTable#, Ptr closure#) encst

pushStatic :: Addr# -> EncodedState -> EncodedState
pushStatic closure# st@EncodedState{encodedStack = xs, encodedNodes} =
  let ptr = Ptr closure#
      encst = st{encodedStack = (Ptr closure#, VacuumStatic):xs, encodedNodes = Map.insert ptr VacuumUndefined encodedNodes}
  in traceShow ("static", Ptr closure#) encst

unsupportedTag :: Word# -> Addr# -> Addr# -> EncodedState -> EncodedState
unsupportedTag _tag# _infoTable# _closure# EncodedState{} =
  error "omg"

yieldPtr :: Word# -> Addr# -> EncodedState -> ST s EncodedState
yieldPtr slot# ptr# st@EncodedState{encodedStack = (k, !v):xs, encodedNodes} = do
  let ptr = Ptr ptr#
      v'  = v{vacuumPayload = Map.insert (W# slot#) (PtrPayload ptr) (vacuumPayload v)}
      st' = st{encodedStack = (k, v'):xs}

  if (Map.member ptr encodedNodes)
    then return st'
    else encodeObject (unsafeCoerce# ptr# :: Any) st'

yieldNPtr :: Word# -> Word# -> EncodedState -> EncodedState
yieldNPtr slot# val# st@EncodedState{encodedStack = (k, !v):xs} =
  let v' = v{vacuumPayload = Map.insert (W# slot#) (NPtrPayload (W# val#)) (vacuumPayload v)}
  in st{encodedStack = (k, v'):xs}

yieldArrWords :: Word# -> ByteArray# -> EncodedState -> EncodedState
yieldArrWords slot# arr# st@EncodedState{encodedStack = (k, !v):xs} =
  let bytes  = fromIntegral $ I# (sizeofByteArray# arr#)
      uarray = UArray 1 bytes (fromIntegral bytes) arr#
      v'     = v{vacuumPayload = Map.insert (W# slot#) (ArrayPayload uarray) (vacuumPayload v)}
  in st{encodedStack = (k, v'):xs}

data Limbo s a = Limbo {limboPtrs :: STRef s (Set Word), limboNPtrs :: STRef s (Set Word), limboVal :: a}
type Tag = Word
type Ptrs = Word
type NPtrs = Word

getInfoTable :: a -> ST s (Ptr InfoTable)
getInfoTable val = ST $ \ st ->
  case unsafeGetInfoTable (unsafeCoerce# val :: Any) st of
    (# st', itable #) -> (# st', Ptr itable #)

allocateClosure :: Ptr InfoTable -> Ptrs -> NPtrs -> ST s (Ptr Closure)
allocateClosure (Ptr infoTable#) (W# ptrs#) (W# nptrs#) = ST  $ \ st ->
  case unsafeAllocateClosure infoTable# ptrs# nptrs# st of
    (# st', clos# #) -> (# st', Ptr clos# #)

limbo :: forall a s . Ptr InfoTable -> Ptrs -> NPtrs -> ST s (Limbo s a)
limbo infoTable ptrs nptrs = do
  let ptrSet  = Set.fromList [i-1      | i <- [1..ptrs]]
      nptrSet = Set.fromList [i+ptrs-1 | i <- [1..nptrs]]

  Ptr clos# <- allocateClosure infoTable ptrs nptrs

  ptrRef  <- newSTRef ptrSet
  nptrRef <- newSTRef nptrSet

  return Limbo{limboPtrs = ptrRef, limboNPtrs = nptrRef, limboVal = unsafeCoerce# clos# :: a}

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
