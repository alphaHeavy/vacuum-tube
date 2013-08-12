{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples  #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Serialize where

import GHC.Prim
import GHC.Int
import GHC.Word
import GHC.Ptr
import Debug.Trace
import Data.Map.Lazy as Map

data Void

-- newtype EncodedState = EncodedState (Map (Ptr Void) Any)
newtype EncodedState = EncodedState (Map (Ptr Void) Int)

instance Show EncodedState where
  show (EncodedState m) = show $ Map.keys m

foreign import prim "Serializze_encodeObject" unsafeEncodeObject :: Any -> Any -> (# Any #)

encodeObject :: a -> EncodedState -> EncodedState
encodeObject val st =
  case unsafeEncodeObject (unsafeCoerce# val :: Any) (unsafeCoerce# st :: Any) of
    (# res #) -> unsafeCoerce# res :: EncodedState

popTag :: EncodedState -> EncodedState
popTag x = trace "pop" x

pushTag :: Word# -> Addr# -> EncodedState -> EncodedState
pushTag tag entryCode x = traceShow ("push", W# tag, Ptr entryCode) x

yieldPtr :: Addr# -> EncodedState -> EncodedState
yieldPtr ptr es@(EncodedState st) | traceShow ("ptr", Ptr ptr) True =
  let combiner _ v _ = v
      p = Ptr ptr
  in case Map.insertLookupWithKey combiner p (0 :: Int) st of
    (Just _,  _  ) -> es
    (Nothing, st') -> encodeObject p (EncodedState st')

yieldNPtr :: Word# -> EncodedState -> EncodedState
yieldNPtr val x = traceShow ("nptr", W# val) x
