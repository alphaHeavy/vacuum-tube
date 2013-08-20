{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples  #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Serialize where

import GHC.Prim
import GHC.Types
import GHC.Word
import GHC.Ptr
import Data.Map.Lazy as Map
import Data.Monoid (mappend)
import Data.Set as Set
import Data.Word
import Debug.Trace

import Data.ByteString (ByteString)
import Control.Monad.ST
import Data.Array.Base

data Closure

instance Show Closure where
  show _ = "Void"

data InfoTable

data Payload
  = PtrPayload (Ptr Closure)
  | NPtrPayload Word
  | ArrayPayload (UArray Word Word8)
    deriving (Eq, Ord, Show)

-- newtype EncodedState = EncodedState (Map (Ptr Void) Any)
data EncodedState = EncodedState [(Tag, Ptr InfoTable, Ptr Closure)] (Map (Ptr Closure) (Map Word Payload))
  deriving Show

foreign import prim "Serializze_encodeObject" unsafeEncodeObject :: Any -> Any -> (# Any #)

encodeObject :: a -> EncodedState -> EncodedState
encodeObject val st =
  case unsafeEncodeObject (unsafeCoerce# val :: Any) (unsafeCoerce# st :: Any) of
    (# res #) -> unsafeCoerce# res :: EncodedState

popTag :: EncodedState -> EncodedState
popTag (EncodedState (_:stack) st) =
  trace "pop" (EncodedState stack st)

pushTag :: Word# -> Addr# -> Addr# -> EncodedState -> EncodedState
pushTag tag infoTable entryCode (EncodedState stack st) =
  traceShow ("push", W# tag, Ptr infoTable, Ptr entryCode)
  (EncodedState ((W# tag, Ptr infoTable, Ptr entryCode):stack) st)

yieldPtr :: Word# -> Addr# -> EncodedState -> EncodedState
yieldPtr slot ptr (EncodedState stack@((_, _, top):_) st) | traceShow ("ptr", Ptr ptr) True =
  let ptr' = Ptr ptr
      payload = PtrPayload ptr'
      addr = unsafeCoerce# ptr :: Any
      insertPayload
        | Map.member ptr' st' = EncodedState stack st'
        | otherwise = encodeObject addr (EncodedState stack (Map.insertWith mappend ptr' Map.empty st'))
        where st' = Map.insertWith mappend top (Map.singleton (W# slot) payload) st

  in insertPayload

yieldNPtr :: Word# -> Word# -> EncodedState -> EncodedState
yieldNPtr slot val (EncodedState stack@((_, _, top):_) st) = traceShow ("nptr", W# val)
  (EncodedState stack (Map.insertWith mappend top (Map.singleton (W# slot) (NPtrPayload (W# val))) st))

yieldArrWords :: Word# -> ByteArray# -> EncodedState -> EncodedState
yieldArrWords slot arr (EncodedState stack@((_, _, top):_) st) =
  let bytes  = fromIntegral $ I# (sizeofByteArray# arr)
      uarray = UArray 1 bytes (fromIntegral bytes) arr
      set'   = Map.singleton (W# slot) (ArrayPayload uarray)
  in traceShow ("bytes", bytes, Ptr (unsafeCoerce# arr))
  (EncodedState stack (Map.insertWith mappend top set' st))
