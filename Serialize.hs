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

data VacuumNode = VacuumNode{vacuumInfoTable :: Ptr InfoTable, vacuumClosure :: Ptr Closure}
  deriving (Eq, Ord, Generic, Show)

instance Binary VacuumNode

newtype VacuumTube a = VacuumTube{unVacuumTube :: a}

buildRemap :: Map (Ptr Closure) (Limbo s Any) -> VacuumNode -> ST s (Map (Ptr Closure) (Limbo s Any))
buildRemap remap VacuumNode{vacuumInfoTable = infoTable, vacuumClosure = closure} = do
  l <- limbo infoTable
  return $! Map.insert closure l remap

keysDifference :: Ord a => Map a b -> Map a c -> Set a
keysDifference x y = Set.difference (Map.keysSet x) (Map.keysSet y)

instance Binary (VacuumTube a) where
  put (VacuumTube val) =
    let st = EncodedState (error "undefined root") [] Set.empty Map.empty
    in put $ runST $ encodeObject val st

  get = do
    EncodedState root [] set map <- get
    runST $ do
      remap <- foldlM buildRemap Map.empty set
      if not (Set.null (keysDifference remap map))
        then return $ fail "Leftover keys"
        else do
          for_ (Map.intersectionWith (,) remap map) $ \ (l, val) ->
            for_ (Map.toList val) $ \ (k, v) ->
              case v of
                PtrPayload p
                  | Just p' <- Map.lookup p remap -> setPtr l k p'
                  | otherwise -> fail "hum, missing pointer payload"
                NPtrPayload p -> setNPtr l k p
                ArrayPayload _ -> return ()

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

foreign import prim "Serializze_encodeObject" unsafeEncodeObject :: Any -> Any -> State# s -> (# State# s, Any #)
foreign import prim "Serializze_allocateClosure" unsafeAllocateClosure :: Addr# -> State# s -> (# State# s, Addr#, Word#, Word# #)
foreign import prim "Serializze_setPtr" unsafeSetPtr :: Any -> Word# -> Any -> State# s -> (# State# s #)
foreign import prim "Serializze_setNPtr" unsafeSetNPtr :: Any -> Word# -> Word# -> State# s -> (# State# s #)
foreign import prim "Serializze_getInfoTable" unsafeGetInfoTable :: Any -> State# s -> (# State# s, Addr# #)

encodeObject :: a -> EncodedState -> ST s EncodedState
encodeObject val enc = ST $ \ st ->
  case unsafeEncodeObject (unsafeCoerce# val :: Any) (unsafeCoerce# enc :: Any) st of
    (# st', res #) -> (# st', unsafeCoerce# res :: EncodedState #)

popTag :: EncodedState -> EncodedState
popTag (EncodedState root (_:stack) erry st) =
  trace "pop" (EncodedState root stack erry st)

pushTag :: Word# -> Addr# -> Addr# -> EncodedState -> EncodedState
pushTag tag# infoTable# entryCode# (EncodedState root stack erry st) =
  let self = VacuumNode (Ptr infoTable#) (Ptr entryCode#)
      root' | List.null stack = self
            | otherwise = root
      encst = EncodedState root' (self:stack) (Set.insert self erry) (Map.insert (Ptr entryCode#) Map.empty st)
  in traceShow ("push", W# tag#, Ptr infoTable#, Ptr entryCode#) encst

unsupportedTag :: Word# -> Addr# -> Addr# -> EncodedState -> EncodedState
unsupportedTag _tag# _infoTable# _entryCode# (EncodedState _root _stack _erry _st) =
  error "omg"

yieldPtr :: Word# -> Addr# -> EncodedState -> ST s EncodedState
yieldPtr slot# ptr# (EncodedState root stack@(VacuumNode _ top:_) erry st) | traceShow ("ptr", Ptr ptr#) True =
  let ptr = Ptr ptr#
      payload = PtrPayload ptr
      addr = unsafeCoerce# ptr# :: Any
      insertPayload
        | Map.member ptr st' = return $ EncodedState root stack erry st'
        | otherwise = encodeObject addr (EncodedState root stack erry (Map.insertWith mappend ptr Map.empty st'))
        where st' = Map.insertWith mappend top (Map.singleton (W# slot#) payload) st

  in insertPayload

yieldNPtr :: Word# -> Word# -> EncodedState -> EncodedState
yieldNPtr slot# val# (EncodedState root stack@(VacuumNode _ top:_) erry st) = traceShow ("nptr", W# val#)
  (EncodedState root stack erry (Map.insertWith mappend top (Map.singleton (W# slot#) (NPtrPayload (W# val#))) st))

yieldArrWords :: Word# -> ByteArray# -> EncodedState -> EncodedState
yieldArrWords slot# arr# (EncodedState root stack@(VacuumNode _ top:_) erry st) =
  let bytes  = fromIntegral $ I# (sizeofByteArray# arr#)
      uarray = UArray 1 bytes (fromIntegral bytes) arr#
      set'   = Map.singleton (W# slot#) (ArrayPayload uarray)
  in traceShow ("bytes", bytes, Ptr (unsafeCoerce# arr#))
  (EncodedState root stack erry (Map.insertWith mappend top set' st))

data Limbo s a = Limbo {limboPtrs :: STRef s (Set Word), limboNPtrs :: STRef s (Set Word), limboVal :: a}
type Tag = Word
type Ptrs = Word
type NPtrs = Word

dup :: a -> ST s a
dup val = do
  itable <- getInfoTable val
  l <- limbo itable
  mval <- ascend l
  return $ fromMaybe (error "dup failed!") mval

getInfoTable :: a -> ST s (Ptr InfoTable)
getInfoTable val = ST $ \ st ->
  case unsafeGetInfoTable (unsafeCoerce# val :: Any) st of
    (# st', itable #) -> (# st', Ptr itable #)

allocateClosure :: Ptr InfoTable -> ST s (Ptr Closure, Word, Word)
allocateClosure (Ptr infoTable#) = ST  $ \ st ->
  case unsafeAllocateClosure infoTable# st of
    (# st', clos#, ptrs#, nptrs# #) -> (# st', (Ptr clos#, W# ptrs#, W# nptrs#) #)

limbo :: forall a s . Ptr InfoTable -> ST s (Limbo s a)
limbo infoTable = do
  unsafeIOToST $ print ("allocating", infoTable)

  (clos@(Ptr clos#), ptrs, nptrs) <- allocateClosure infoTable

  let ptrSet  = Set.fromList [i-1      | i <- [1..ptrs]]
      nptrSet = Set.fromList [i+ptrs-1 | i <- [1..nptrs]]

  unsafeIOToST $ print ("allocated", clos, ptrSet, nptrSet)

  ptrRef  <- newSTRef ptrSet
  nptrRef <- newSTRef nptrSet

  return Limbo{limboPtrs = ptrRef, limboNPtrs = nptrRef, limboVal = unsafeCoerce# clos# :: a}

setPtr :: Limbo s a -> Word -> Limbo s Any -> ST s ()
setPtr Limbo{limboPtrs = ptrSet, limboVal = closure} slot@(W# slot#) Limbo{limboVal = val}
  | traceShow ("setPtr", slot) False = undefined
  | otherwise = do
      ST $ \ st -> case unsafeSetPtr (unsafeCoerce# closure :: Any) slot# val st of
        (# st' #) -> (# st', () #)

      modifySTRef' ptrSet (Set.delete slot)

setNPtr :: Limbo s a -> Word -> Word -> ST s ()
setNPtr Limbo{limboNPtrs = nptrRef, limboVal = closure} slot@(W# slot#) (W# val#) = do
  ST $ \ st -> case unsafeSetNPtr (unsafeCoerce# closure :: Any) slot# val# st of
    (# st' #) -> (# st', () #)

  modifySTRef' nptrRef (Set.delete slot)

ascend :: forall a s . Limbo s Any -> ST s (Maybe a)
ascend Limbo{limboPtrs = ptrRef, limboNPtrs = nptrRef, limboVal = val} = do
  ptrSet <- readSTRef ptrRef
  nptrSet <- readSTRef nptrRef
  return $ case (Set.null ptrSet, Set.null nptrSet) of
    (True, True) -> Just (unsafeCoerce# val :: a)
    _            -> Nothing
