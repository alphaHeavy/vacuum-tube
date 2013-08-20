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
data EncodedState = EncodedState [Ptr Closure] (Map (Ptr Closure) (Set Payload))
  deriving Show

foreign import prim "Serializze_encodeObject" unsafeEncodeObject :: Any -> Any -> (# Any #)

encodeObject :: a -> EncodedState -> EncodedState
encodeObject val st =
  case unsafeEncodeObject (unsafeCoerce# val :: Any) (unsafeCoerce# st :: Any) of
    (# res #) -> unsafeCoerce# res :: EncodedState

popTag :: EncodedState -> EncodedState
popTag (EncodedState (_:stack) st) =
  trace "pop" (EncodedState stack st)

pushTag :: Word# -> Addr# -> EncodedState -> EncodedState
pushTag tag entryCode (EncodedState stack st) =
  traceShow ("push", W# tag, Ptr entryCode)
  (EncodedState (Ptr entryCode:stack) st)

yieldPtr :: Addr# -> EncodedState -> EncodedState
yieldPtr ptr (EncodedState stack@(top:_) st) | traceShow ("ptr", Ptr ptr) True =
  let ptr' = Ptr ptr
      payload = PtrPayload ptr'
      addr = unsafeCoerce# ptr :: Any
      insertPayload
        | Map.member ptr' st' = EncodedState stack st'
        | otherwise = encodeObject addr (EncodedState stack (Map.insertWith mappend ptr' Set.empty st'))
        where st' = Map.insertWith mappend top (Set.singleton payload) st

  in insertPayload

yieldNPtr :: Word# -> EncodedState -> EncodedState
yieldNPtr val (EncodedState stack@(top:_) st) = traceShow ("nptr", W# val)
  (EncodedState stack (Map.insertWith mappend top (Set.singleton (NPtrPayload (W# val))) st))

yieldArrWords :: ByteArray# -> EncodedState -> EncodedState
yieldArrWords arr (EncodedState stack@(top:_) st) =
  let bytes  = sizeofByteArray# arr
      bytes' = fromIntegral $ I# bytes
      uarray = UArray 1 bytes' (fromIntegral bytes') arr
      set'   = Set.singleton (ArrayPayload uarray)
  in traceShow ("bytes", bytes', Ptr (unsafeCoerce# arr))
  (EncodedState stack (Map.insertWith mappend top set' st))
