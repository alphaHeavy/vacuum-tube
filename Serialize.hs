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
encodeObject !val st =
  case unsafeEncodeObject (unsafeCoerce# val :: Any) (unsafeCoerce# st :: Any) of
    (# res #) -> unsafeCoerce# res :: EncodedState

popTag :: EncodedState -> EncodedState
popTag x = trace "pop" x

pushTag :: Word# -> Addr# -> EncodedState -> EncodedState
pushTag tag entryCode x = traceShow ("push", W64# tag, Ptr entryCode) x

yieldPtr :: Addr# -> EncodedState -> EncodedState
-- yieldPtr ptr (EncodedState st) = trace "ptr" (EncodedState (Map.insert (Ptr (unsafeCoerce# ptr)) ptr st))
yieldPtr ptr (EncodedState st) = trace "ptr" (EncodedState (Map.insert (Ptr ptr) (0 :: Int) st))

yieldNPtr :: Word# -> EncodedState -> EncodedState
yieldNPtr val x = traceShow ("nptr", W64# val) x
