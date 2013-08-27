{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples  #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Serialize where

import Control.Applicative
import Data.Array.Base
import Data.Binary
import Data.Foldable
import Data.Map.Lazy as Map
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
  put (VacuumPtr (Ptr val)) = put (W# (unsafeCoerce# val))
  get = do
    W# val <- get
    return . VacuumPtr $ Ptr (unsafeCoerce# val)

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

data Errythang = Errythang{erryTag :: Tag, erryInfoTable :: Ptr InfoTable, erryClosure :: Ptr Closure}
  deriving (Eq, Ord, Generic, Show)

instance Binary Errythang

newtype VacuumTube a = VacuumTube{unVacuumTube :: a}

instance Binary (VacuumTube a) where
  put (VacuumTube val) =
    let st = EncodedState [] Set.empty Map.empty
    in put $ encodeObject val st

  get = do
    EncodedState [] set map <- get
    runST $ do
      root <- newSTRef (error "whynoref?")
      for_ set $ \ Errythang{erryInfoTable = infoTable, erryClosure = clos} -> do
        l <- limbo infoTable
        case Map.lookup clos map of
          Nothing -> error "omg wtf"
          Just val -> do
            for_ (Map.toList val) $ \ (k, v) ->
              case v of
                PtrPayload (Ptr p) -> setPtr l k (unsafeCoerce# p :: Any)
                NPtrPayload p -> setNPtr l k p
                ArrayPayload _ -> return ()

            mval <- ascend l
            case mval of
              Just complete -> writeSTRef root complete
              Nothing -> error "partially constructed value"

      readSTRef root

data EncodedState = EncodedState [Errythang] (Set Errythang) (Map (Ptr Closure) (Map Word Payload))
  deriving (Show, Generic)

instance Binary EncodedState

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

data Limbo s a = Limbo {limboPtrs :: STRef s (Set Word), limboNPtrs :: STRef s (Set Word), limboVal :: a}
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

  return Limbo{limboPtrs = ptrSet, limboNPtrs = nptrSet, limboVal = unsafeCoerce# clos}

setPtr :: Limbo s a -> Word -> Any -> ST s ()
setPtr Limbo{limboPtrs = ptrSet, limboVal = closure} slot'@(W# slot) val
  | traceShow ("setPtr", slot') False = undefined
  | otherwise = do
      ST $ \ st -> case unsafeSetPtr (unsafeCoerce# closure) slot val st of
        (# st' #) -> (# st', () #)

      modifySTRef' ptrSet (Set.delete slot')

setNPtr :: Limbo s a -> Word -> Word -> ST s ()
setNPtr Limbo{limboNPtrs = nptrSet, limboVal = closure} slot'@(W# slot) (W# val) = do
  ST $ \ st -> case unsafeSetNPtr (unsafeCoerce# closure) slot val st of
    (# st' #) -> (# st', () #)

  modifySTRef' nptrSet (Set.delete slot')

ascend :: Limbo s a -> ST s (Maybe a)
ascend Limbo{limboPtrs = ptrRef, limboNPtrs = nptrRef, limboVal = val} = do
  ptrSet <- readSTRef ptrRef
  nptrSet <- readSTRef nptrRef
  return $ case (Set.null ptrSet, Set.null nptrSet) of
    (True, True) -> Just (unsafeCoerce# val)
    _            -> Nothing
