{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples  #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Serialize where

import GHC.Prim
import GHC.Ptr
import GHC.ST
import Data.STRef
import GHC.Types
import GHC.Word
import Data.Set as Set
import Data.Map.Lazy as Map
import Data.Monoid (mappend)
import Debug.Trace

import Data.Array.Base
import Data.Binary

data Closure

instance Show Closure where
  show _ = "Void"

data InfoTable

data Payload
  = PtrPayload (Ptr Closure)
  | NPtrPayload Word
  | ArrayPayload (UArray Word Word8)
    deriving (Eq, Ord, Show)

data Errythang = Errythang{erryTag :: Tag, erryInfoTable :: Ptr InfoTable, erryClosure :: Ptr Closure}
  deriving (Eq, Ord, Show)

newtype VacuumTube a = VacuumTube{unVacuumTube :: a}

instance Binary (VacuumTube a) where
  put _ = undefined
  get = undefined

data EncodedState = EncodedState [Errythang] (Set Errythang) (Map (Ptr Closure) (Map Word Payload))
  deriving Show

foreign import prim "Serializze_encodeObject" unsafeEncodeObject :: Any -> Any -> (# Any #)
foreign import prim "Serializze_allocateClosure" unsafeAllocateClosure :: Addr# -> State# s -> (# State# s, Any, Word#, Word# #)
foreign import prim "Serializze_setPtr" unsafeSetPtr :: Any -> Word# -> Any -> State# s -> (# State# s #)
foreign import prim "Serializze_setNPtr" unsafeSetNPtr :: Any -> Word# -> Word# -> State# s -> (# State# s #)

encodeObject :: a -> EncodedState -> EncodedState
encodeObject val st =
  case unsafeEncodeObject (unsafeCoerce# val) (unsafeCoerce# st) of
    (# res #) -> unsafeCoerce# res

popTag :: EncodedState -> EncodedState
popTag (EncodedState (_:stack) erry st) =
  trace "pop" (EncodedState stack erry st)

pushTag :: Word# -> Addr# -> Addr# -> EncodedState -> EncodedState
pushTag tag infoTable entryCode (EncodedState stack erry st) =
  let self = Errythang (W# tag) (Ptr infoTable) (Ptr entryCode)
      encst = EncodedState (self:stack) (Set.insert self erry) st
  in traceShow ("push", W# tag, Ptr infoTable, Ptr entryCode) encst

unsupportedTag :: Word# -> Addr# -> Addr# -> EncodedState -> EncodedState
unsupportedTag tag infoTable entryCode (EncodedState stack erry st) =
  error "omg"

yieldPtr :: Word# -> Addr# -> EncodedState -> EncodedState
yieldPtr slot ptr (EncodedState stack@(Errythang _ _ top:_) erry st) | traceShow ("ptr", Ptr ptr) True =
  let ptr' = Ptr ptr
      payload = PtrPayload ptr'
      addr = unsafeCoerce# ptr :: Any
      insertPayload
        | Map.member ptr' st' = EncodedState stack erry st'
        | otherwise = encodeObject addr (EncodedState stack erry (Map.insertWith mappend ptr' Map.empty st'))
        where st' = Map.insertWith mappend top (Map.singleton (W# slot) payload) st

  in insertPayload

yieldNPtr :: Word# -> Word# -> EncodedState -> EncodedState
yieldNPtr slot val (EncodedState stack@(Errythang _ _ top:_) erry st) = traceShow ("nptr", W# val)
  (EncodedState stack erry (Map.insertWith mappend top (Map.singleton (W# slot) (NPtrPayload (W# val))) st))

yieldArrWords :: Word# -> ByteArray# -> EncodedState -> EncodedState
yieldArrWords slot arr (EncodedState stack@(Errythang _ _ top:_) erry st) =
  let bytes  = fromIntegral $ I# (sizeofByteArray# arr)
      uarray = UArray 1 bytes (fromIntegral bytes) arr
      set'   = Map.singleton (W# slot) (ArrayPayload uarray)
  in traceShow ("bytes", bytes, Ptr (unsafeCoerce# arr))
  (EncodedState stack erry (Map.insertWith mappend top set' st))

data Limbo s a = Limbo {limboPtrs :: STRef s (Set Word), limboNPtrs :: STRef s (Set Word), limboVal :: Any}
type Tag = Word
type Ptrs = Word
type NPtrs = Word

limbo :: Ptr InfoTable -> ST s (Limbo s a)
limbo (Ptr infoTable) = do
  (clos, ptrs, nptrs) <- ST $ \ st ->
    case unsafeAllocateClosure infoTable st of
      (# st', clos, ptrs, nptrs #) -> (# st', (clos, W# ptrs, W# nptrs) #)

  ptrSet  <- newSTRef $ Set.fromList [i-1 | i <- [1..ptrs]]
  nptrSet <- newSTRef $ Set.fromList [i+ptrs-1 | i <- [1..nptrs]]

  return Limbo{limboPtrs = ptrSet, limboNPtrs = nptrSet, limboVal = clos}

setPtr :: Limbo s a -> Word -> a -> ST s ()
setPtr Limbo{limboPtrs = ptrSet, limboVal = closure} slot'@(W# slot) val = do
  ST $ \ st -> case unsafeSetPtr closure slot (unsafeCoerce# val) st of
    (# st' #) -> (# st', () #)

  modifySTRef' ptrSet (Set.delete slot')

setNPtr :: Limbo s a -> Word -> Word -> ST s ()
setNPtr Limbo{limboNPtrs = nptrSet, limboVal = closure} slot'@(W# slot) (W# val) = do
  ST $ \ st -> case unsafeSetNPtr closure slot val st of
    (# st' #) -> (# st', () #)

  modifySTRef' nptrSet (Set.delete slot')

ascend :: Limbo s a -> ST s (Maybe a)
ascend Limbo{limboPtrs = ptrRef, limboNPtrs = nptrRef, limboVal = val} = do
  ptrSet <- readSTRef ptrRef
  nptrSet <- readSTRef nptrRef
  return $ case (Set.null ptrSet, Set.null nptrSet) of
    (True, True) -> Just (unsafeCoerce# val)
    _            -> Nothing
