{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
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
  deriving Generic

instance Show Closure where
  show _ = "Void"

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
  = VacuumNode{vacuumInfoTable :: Ptr InfoTable, vacuumPtrs :: Word, vacuumNPtrs :: Word, vacuumClosure :: Ptr Closure}
  | VacuumStatic{vacuumClosure :: Ptr Closure}
  deriving (Eq, Ord, Generic, Show)

instance Binary VacuumNode

newtype VacuumTube a = VacuumTube{unVacuumTube :: a}

buildRemap :: Map (Ptr Closure) (Limbo s Any) -> VacuumNode -> ST s (Map (Ptr Closure) (Limbo s Any))
buildRemap remap VacuumNode{vacuumInfoTable = infoTable, vacuumPtrs = ptrs, vacuumNPtrs = nptrs, vacuumClosure = closure} = do
  unsafeIOToST . traceIO $ show ("limbo", infoTable, closure)
  l <- limbo infoTable ptrs nptrs
  return $! Map.insert closure l remap

buildRemap remap VacuumStatic{vacuumClosure = closure} = do
  unsafeIOToST . traceIO $ show ("lstatic", closure)
  l <- limboStatic closure
  return $! Map.insert closure l remap

keysDifference :: Ord a => Map a b -> Map a c -> Set a
keysDifference x y = Map.keysSet x `Set.difference` Map.keysSet y

instance Binary (VacuumTube a) where
  put (VacuumTube val) =
    let st = EncodedState (error "undefined root") [] Set.empty Map.empty
    in put $ runST $ encodeObject val st

  get = do
    EncodedState root [] nodeSet nodeMap <- get
    traceShow ("root", root, nodeSet, nodeMap) $ runST $ do
      remap <- foldlM buildRemap Map.empty nodeSet
      if not (Set.null (keysDifference remap nodeMap))
        then return $ fail "Leftover keys"
        else trace ("Remap: " ++ show (Map.keys remap)) $ do
          unsafeIOToST . traceIO $ "Top: " ++ show (Map.size (Map.intersectionWith (,) remap nodeMap))
          for_ (Map.intersectionWith (,) remap nodeMap) $ \ (l, val) -> do
            unsafeIOToST . traceIO $ "Per: " ++ show (Map.size val)
            for_ (Map.toList val) $ \ (k, v) -> do
              unsafeIOToST . traceIO $ "Checking: " ++ show v
              case v of
                PtrPayload p
                  | Just p' <- Map.lookup p remap -> trace ("ptr match") $ setPtr l k p'
                  | otherwise -> trace ("ptr nomatch") $ error "foo" -- setPtr l k (vacuumInfoTable 
                NPtrPayload p -> setNPtr l k p
                ArrayPayload (UArray _ _ _ p) -> setByteArray l k p

          case Map.lookup (vacuumClosure root) remap of
            Just limboRoot -> do
              mval <- ascend limboRoot
              case mval of
                Just val -> return $ return (VacuumTube val :: VacuumTube a)
                Nothing  -> return $ fail "Ascension failed"
            Nothing -> return $ fail "Root closure not found after construction"

data EncodedState = EncodedState
  { encodedRoot  :: VacuumNode
  , encodedStack :: [VacuumNode]
  , encodedNodes :: Set VacuumNode
  , encodedPayloads :: Map (Ptr Closure) (Map Word Payload)
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
popTag (EncodedState root (top:stack) erry st) =
  traceShow ("pop", top) (EncodedState root stack erry st)

pushTag :: Word# -> Addr# -> Word# -> Word# -> Addr# -> EncodedState -> EncodedState
pushTag tag# infoTable# ptrs# nptrs# closure# (EncodedState root stack erry st) =
  let self = VacuumNode (Ptr infoTable#) (W# ptrs#) (W# nptrs#) (Ptr closure#)
      root' | List.null stack = self
            | otherwise = root
      encst = EncodedState root' (self:stack) (Set.insert self erry) (Map.insert (Ptr closure#) Map.empty st)
  in traceShow ("push", W# tag#, Ptr infoTable#, Ptr closure#) encst

pushStatic :: Addr# -> EncodedState -> EncodedState
pushStatic closure# (EncodedState root stack erry st) =
  let self = VacuumStatic (Ptr closure#)
      root' | List.null stack = self
            | otherwise = root
      encst = EncodedState root' (self:stack) (Set.insert self erry) (Map.insert (Ptr closure#) Map.empty st)
  in traceShow ("static", Ptr closure#) encst

unsupportedTag :: Word# -> Addr# -> Addr# -> EncodedState -> EncodedState
unsupportedTag _tag# _infoTable# _closure# EncodedState{} =
  error "omg"

yieldPtr :: Word# -> Addr# -> EncodedState -> ST s EncodedState
yieldPtr slot# ptr# (EncodedState root stack@(VacuumNode{vacuumClosure = top}:_) erry st) | traceShow ("ptr", Ptr ptr#) True =
  let ptr = Ptr ptr#
      payload = PtrPayload ptr
      addr = unsafeCoerce# ptr# :: Any
      insertPayload
        | Map.member ptr st' = return $ EncodedState root stack erry st'
        | otherwise = encodeObject addr (EncodedState root stack erry (Map.insertWith mappend ptr Map.empty st'))
        where st' = Map.insertWith mappend top (Map.singleton (W# slot#) payload) st

  in insertPayload

yieldNPtr :: Word# -> Word# -> EncodedState -> EncodedState
yieldNPtr slot# val# (EncodedState root stack@(VacuumNode{vacuumClosure = top}:_) erry st) = traceShow ("nptr", W# val#)
  (EncodedState root stack erry (Map.insertWith mappend top (Map.singleton (W# slot#) (NPtrPayload (W# val#))) st))

yieldArrWords :: Word# -> ByteArray# -> EncodedState -> EncodedState
yieldArrWords slot# arr# (EncodedState root stack@(VacuumNode{vacuumClosure = top}:_) erry st) =
  let bytes  = fromIntegral $ I# (sizeofByteArray# arr#)
      uarray = UArray 1 bytes (fromIntegral bytes) arr#
      set'   = Map.singleton (W# slot#) (ArrayPayload uarray)
  in traceShow ("bytes", bytes, Ptr (unsafeCoerce# arr#))
  (EncodedState root stack erry (Map.insertWith mappend top set' st))

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
  unsafeIOToST . traceIO $ show ("allocating", infoTable)

  clos@(Ptr clos#) <- allocateClosure infoTable ptrs nptrs

  let ptrSet  = Set.fromList [i-1      | i <- [1..ptrs]]
      nptrSet = Set.fromList [i+ptrs-1 | i <- [1..nptrs]]

  unsafeIOToST . traceIO $ show ("allocated", clos, ptrSet, nptrSet)

  ptrRef  <- newSTRef ptrSet
  nptrRef <- newSTRef nptrSet

  return Limbo{limboPtrs = ptrRef, limboNPtrs = nptrRef, limboVal = unsafeCoerce# clos# :: a}

limboStatic :: forall a s . Ptr Closure -> ST s (Limbo s a)
limboStatic (Ptr clos#) = do
  ptrRef  <- newSTRef $ Set.empty
  nptrRef <- newSTRef $ Set.empty

  return Limbo{limboPtrs = ptrRef, limboNPtrs = nptrRef, limboVal = unsafeCoerce# clos# :: a}

setByteArray :: Limbo s a -> Word -> ByteArray# -> ST s ()
setByteArray Limbo{limboPtrs = ptrRef, limboVal = closure} slot arr# = do
  ST $ \ st -> case unsafeIndirectByteArray (unsafeCoerce# closure :: Any) arr# st of
    (# st' #) -> (# st', () #)

  modifySTRef' ptrRef (Set.delete slot)

setPtr :: Limbo s a -> Word -> Limbo s Any -> ST s ()
{-# NOINLINE setPtr #-}
setPtr Limbo{limboPtrs = ptrRef, limboVal = closure} slot@(W# slot#) Limbo{limboVal = val}
  -- | traceShow ("setPtr", Ptr (unsafeCoerce# closure), slot, Ptr (unsafeCoerce# val)) False = undefined
  | otherwise = do
      unsafeIOToST . putStrLn $ "setPtr begin"

      ST $ \ st -> case unsafeSetPtr (unsafeCoerce# closure :: Any) slot# val st of
        (# st' #) -> (# st', () #)

      unsafeIOToST . putStrLn $ "setPtr done"

      modifySTRef' ptrRef (Set.delete slot)

setNPtr :: Limbo s a -> Word -> Word -> ST s ()
setNPtr Limbo{limboNPtrs = nptrRef, limboVal = closure} slot@(W# slot#) val@(W# val#)
  -- | traceShow ("setPtr", Ptr (unsafeCoerce# closure), slot, val) False = undefined
  | otherwise = do
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
